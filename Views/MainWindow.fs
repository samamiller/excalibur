namespace Excalibur.Views

open System
open System.Collections.Generic
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Platform.Storage
open Avalonia.Platform.Storage.FileIO
open Excalibur.Dialogs
open Excalibur.Domain
open Excalibur.Services
open Excalibur.Views.TopBar
open Excalibur.Views.BookList
open Excalibur.Views.EditBar
open Excalibur.Views.TagBrowser
open Excalibur.Views.BookDetails

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Excalibur"
        base.Width <- 1280
        base.Height <- 800
        LibraryService.initialize ()

        let loadBooks () = LibraryService.loadBooks ()

        this.Content <-
            Component (fun ctx ->
                let booksState = ctx.useState (loadBooks ())
                let query = ctx.useState ""
                let selectedIds = ctx.useState ([]: int list)
                let theme = ctx.useState "Light"

                let filteredBooks =
                    let books = booksState.Current

                    if String.IsNullOrWhiteSpace query.Current then
                        books
                    else
                        let q = query.Current.Trim().ToLowerInvariant()

                        books
                        |> List.filter (fun b ->
                            b.title.ToLowerInvariant().Contains q
                            || (b.author
                                |> Option.defaultValue ""
                                |> fun a -> a.ToLowerInvariant().Contains q))

                let refreshBooks () =
                    booksState.Set(loadBooks ())
                    selectedIds.Set []

                let addFromPath (file: string) =
                    LibraryService.addBookFromPath file None
                    refreshBooks ()

                let refreshAndMarkMissing () =
                    LibraryService.syncMissingFlags ()
                    refreshBooks ()

                let handleBatchEdit () =
                    async {
                        let! result = EditMetadataDialog.show this

                        match result with
                        | Some dialogResult ->
                            LibraryService.updateAuthorTags selectedIds.Current dialogResult.Author dialogResult.Tags

                            refreshBooks ()
                        | None -> ()
                    }
                    |> Async.StartImmediate

                let topBar =
                    TopBar.view
                        { Query = query.Current
                          OnQueryChanged =
                            (fun txt ->
                                query.Set txt
                                selectedIds.Set [])
                          OnClear =
                            (fun () ->
                                query.Set ""
                                selectedIds.Set [])
                          OnAddBooks =
                            (fun () ->
                                async {
                                    let options = FilePickerOpenOptions()
                                    options.AllowMultiple <- true

                                    let ebookType = FilePickerFileType "E-books"

                                    ebookType.Patterns <-
                                        List<string>(
                                            [ "*.epub"
                                              "*.pdf"
                                              "*.mobi"
                                              "*.azw3" ]
                                        )

                                    options.FileTypeFilter <- List<FilePickerFileType>([ ebookType ])

                                    let! storageFiles =
                                        this.StorageProvider.OpenFilePickerAsync(options)
                                        |> Async.AwaitTask

                                    storageFiles
                                    |> Seq.choose (fun file ->
                                        let path = file.TryGetLocalPath()

                                        if String.IsNullOrWhiteSpace path then
                                            None
                                        else
                                            Some path)
                                    |> Seq.iter addFromPath

                                    refreshAndMarkMissing ()
                                }
                                |> Async.StartImmediate)
                          OnImportFolder =
                            (fun () ->
                                async {
                                    let options = FolderPickerOpenOptions()
                                    options.AllowMultiple <- false

                                    let! storageFolders =
                                        this.StorageProvider.OpenFolderPickerAsync(options)
                                        |> Async.AwaitTask

                                    match storageFolders |> Seq.tryHead with
                                    | None -> ()
                                    | Some folder ->
                                        let itemsEnum = folder.GetItemsAsync()
                                        let mutable acc = []
                                        let e = itemsEnum.GetAsyncEnumerator()
                                        let mutable hasNext = true

                                        while hasNext do
                                            let! moved = e.MoveNextAsync().AsTask() |> Async.AwaitTask

                                            if moved then
                                                acc <- e.Current :: acc
                                            else
                                                hasNext <- false

                                        do! e.DisposeAsync().AsTask() |> Async.AwaitTask

                                        acc
                                        |> Seq.choose (fun (it: IStorageItem) ->
                                            match it with
                                            | :? IStorageFile as sf ->
                                                let path = sf.TryGetLocalPath()

                                                if String.IsNullOrWhiteSpace path then
                                                    None
                                                else
                                                    let ext =
                                                        System
                                                            .IO
                                                            .Path
                                                            .GetExtension(path)
                                                            .ToLowerInvariant()

                                                    if [ ".epub"; ".pdf"; ".mobi"; ".azw3" ]
                                                       |> List.contains ext then
                                                        Some path
                                                    else
                                                        None
                                            | _ -> None)
                                        |> Seq.iter addFromPath

                                        refreshAndMarkMissing ()
                                }
                                |> Async.StartImmediate)
                          Theme = theme.Current
                          OnToggleTheme =
                            (fun () ->
                                let next =
                                    if theme.Current = "Light" then
                                        "Dark"
                                    else
                                        "Light"

                                theme.Set next

                                match next with
                                | "Dark" -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Dark
                                | _ -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Light) }
                // Keyboard delete removes selected books
                this.KeyDown.Add (fun e ->
                    if
                        e.Key = Avalonia.Input.Key.Delete
                        && not (List.isEmpty selectedIds.Current)
                    then
                        LibraryService.deleteBooks selectedIds.Current
                        refreshBooks ())

                let selectedBook =
                    lazy
                        (match selectedIds.Current with
                         | id :: _ ->
                             booksState.Current
                             |> List.tryFind (fun b -> b.id = id)
                         | [] -> None)

                let bookListView =
                    BookList.view
                        { Books = filteredBooks
                          OnSelectionChanged = selectedIds.Set }

                let editBar = EditBar.view { OnBatchEdit = handleBatchEdit }

                let tagBrowser =
                    TagBrowser.view
                        { Books = booksState.Current
                          OnFilterByTag =
                            (fun tagOpt ->
                                match tagOpt with
                                | None -> query.Set ""
                                | Some t -> query.Set t) }

                let detailsPane = BookDetails.view { Selected = selectedBook.Value }

                let threePane =
                    Grid.create [ Grid.rowDefinitions "Auto,*"
                                  Grid.columnDefinitions "240,*,320"
                                  Grid.children [ Grid.create [ Grid.row 0
                                                                Grid.columnSpan 3
                                                                Grid.children [ topBar ] ]
                                                  Grid.create [ Grid.row 1
                                                                Grid.column 0
                                                                Grid.children [ ScrollViewer.create [ ScrollViewer.content
                                                                                                          tagBrowser ] ] ]
                                                  Grid.create [ Grid.row 1
                                                                Grid.column 1
                                                                Grid.children [ ScrollViewer.create [ ScrollViewer.content
                                                                                                          bookListView ] ] ]
                                                  Grid.create [ Grid.row 1
                                                                Grid.column 2
                                                                Grid.children [ Border.create [ Border.child detailsPane ] ] ] ] ]

                DockPanel.create [ DockPanel.children [ threePane
                                                        editBar ] ])
