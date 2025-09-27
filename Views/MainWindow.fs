namespace Excalibur.Views

open System
open System.Collections.Generic
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Platform.Storage
open Excalibur.Dialogs
open Excalibur.Domain
open Excalibur.Services
open Excalibur.Views.TopBar
open Excalibur.Views.BookList
open Excalibur.Views.EditBar

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Excalibur"
        LibraryService.initialize ()

        let loadBooks () = LibraryService.loadBooks ()

        this.Content <-
            Component (fun ctx ->
                let booksState = ctx.useState (loadBooks ())
                let query = ctx.useState ""
                let selectedIds = ctx.useState ([]: int list)

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
                                |> Async.StartImmediate) }

                let bookListView =
                    BookList.view
                        { Books = filteredBooks
                          OnSelectionChanged = selectedIds.Set }

                let editBar = EditBar.view { OnBatchEdit = handleBatchEdit }

                let dropHost: Types.IView =
                    Border.create [ Border.child (
                                        ScrollViewer.create [ DockPanel.dock Dock.Bottom
                                                              ScrollViewer.content bookListView ]
                                    )
                                    Border.onPointerPressed (fun _ -> ()) ]

                DockPanel.create [ DockPanel.children [ topBar
                                                        dropHost
                                                        editBar ] ])
