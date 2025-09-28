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
open Excalibur.Views.BookGrid
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
                let status = ctx.useState (None: string option)

                let filteredBooks =
                    let books = booksState.Current

                    if String.IsNullOrWhiteSpace query.Current then
                        books |> List.sortBy (fun b -> b.title.ToLowerInvariant())
                    else
                        let q = query.Current.Trim().ToLowerInvariant()

                        books
                        |> List.filter (fun b ->
                            b.title.ToLowerInvariant().Contains q
                            || (b.author
                                |> Option.defaultValue ""
                                |> fun a -> a.ToLowerInvariant().Contains q))
                        |> List.sortBy (fun b -> b.title.ToLowerInvariant())

                let refreshBooks () =
                    let loaded = loadBooks ()
                    booksState.Set loaded
                    selectedIds.Set []
                    // Show a small debug status of loaded count
                    status.Set (Some (sprintf "Loaded %d from DB" (loaded |> List.length)))

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

                                    let mutable added = 0
                                    let mutable skipped = 0

                                    storageFiles
                                    |> Seq.choose (fun file ->
                                        let path = file.TryGetLocalPath()

                                        if String.IsNullOrWhiteSpace path then
                                            None
                                        else
                                            Some path)
                                    |> Seq.iter (fun p ->
                                        if LibraryService.addBookFromPath p None then added <- added + 1 else skipped <- skipped + 1)

                                    // Clear filters so newly added books are visible
                                    query.Set ""
                                    refreshAndMarkMissing ()
                                    // Extra refresh to ensure UI state updates after DB ops
                                    booksState.Set(loadBooks ())
                                    status.Set (Some (sprintf "Added %d, skipped %d" added skipped))
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

                                        let rec isBookFile (path: string) =
                                            let ext = System.IO.Path.GetExtension(path).ToLowerInvariant()
                                            [ ".epub"; ".pdf"; ".mobi"; ".azw3" ] |> List.contains ext

                                        let rec enumerate (folder: IStorageFolder) = task {
                                            let items = folder.GetItemsAsync()
                                            let e = items.GetAsyncEnumerator()
                                            let mutable results : string list = []
                                            let mutable hasNext = true
                                            while hasNext do
                                                let! moved = e.MoveNextAsync().AsTask()
                                                if moved then
                                                    match e.Current with
                                                    | :? IStorageFile as f ->
                                                        let p = f.TryGetLocalPath()
                                                        if not (String.IsNullOrWhiteSpace p) && isBookFile p then
                                                            results <- p :: results
                                                    | :? IStorageFolder as sub ->
                                                        let! subFiles = enumerate sub
                                                        results <- List.append subFiles results
                                                    | _ -> ()
                                                else hasNext <- false
                                            do! e.DisposeAsync().AsTask()
                                            return results }

                                        let! files = enumerate folder |> Async.AwaitTask

                                        let mutable added = 0
                                        let mutable skipped = 0
                                        files |> List.iter (fun p ->
                                            if LibraryService.addBookFromPath p None then added <- added + 1 else skipped <- skipped + 1)

                                        // Clear filters so newly imported books are visible
                                        query.Set ""
                                        refreshAndMarkMissing ()
                                        // Extra refresh to ensure UI state updates after DB ops
                                        booksState.Set(loadBooks ())
                                        status.Set (Some (sprintf "Added %d, skipped %d" added skipped))
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
                                | _ -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Light)
                          Status = status.Current }
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

                let initialColumns : Excalibur.Views.BookGrid.ColumnConfig list =
                    [ { Id = ColumnId.Title; Header = "Title" }
                      { Id = ColumnId.Author; Header = "Author" }
                      { Id = ColumnId.ColTags; Header = "Tags" }
                      { Id = ColumnId.Added; Header = "Added" }
                      { Id = ColumnId.Missing; Header = "Missing" } ]

                let columnsState = ctx.useState initialColumns

                let bookListView : Types.IView =
                    // Working fallback: simple ListBox rendering of Title — Author
                    ListBox.create [
                        ListBox.dataItems filteredBooks
                        ListBox.itemTemplate (
                            DataTemplateView<Excalibur.Domain.Book>.create (fun b ->
                                let line = sprintf "%s — %s" b.title (defaultArg b.author "")
                                TextBlock.create [ TextBlock.text line ]
                            )
                        )
                        ListBox.onSelectionChanged (fun args ->
                            match args.Source with
                            | :? ListBox as lb ->
                                lb.SelectedItems
                                |> Seq.cast<Excalibur.Domain.Book>
                                |> Seq.map (fun b -> b.id)
                                |> Seq.toList
                                |> selectedIds.Set
                            | _ -> ()
                        )
                    ]

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
                                                                Grid.children [
                                                                    // Count + simple mirror list to validate data feed
                                                                    StackPanel.create [ StackPanel.spacing 6.0
                                                                                        StackPanel.children [
                                                                                            TextBlock.create [ TextBlock.text (sprintf "Books: %d" (filteredBooks |> List.length)) ]
                                                                                            // Temporary mirror list to validate items rendering path
                                                                                            ListBox.create [ ListBox.height 0.0 ]
                                                                                            bookListView
                                                                                        ] ]
                                                                ] ]
                                                  Grid.create [ Grid.row 1
                                                                Grid.column 2
                                                                Grid.children [ Border.create [ Border.child detailsPane ] ] ] ] ]

                DockPanel.create [ DockPanel.children [ threePane
                                                        editBar ] ])
