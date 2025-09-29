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
open Excalibur.Services.ConfigService

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Excalibur"
        base.Width <- 1280
        base.Height <- 800
        LibraryService.initialize ()

        let loadBooks () = LibraryService.loadBooks ()

        this.Content <-
            Component(fun ctx ->
                let booksState = ctx.useState (loadBooks ())
                let query = ctx.useState ""
                let authorEq = ctx.useState (None: string option)
                let tagEq = ctx.useState (None: string option)
                let seriesEq = ctx.useState (None: string option)
                let totalCount = ctx.useState (booksState.Current.Length)
                let pageSize = 50
                let pageIndex = ctx.useState 0
                let selectedIds = ctx.useState ([]: int list)
                let theme = ctx.useState "System"
                let status = ctx.useState (None: string option)

                // Initialize theme from config
                do
                    let cfg = ConfigService.load ()
                    let desired = cfg.Theme |> Option.defaultValue "System"
                    theme.Set desired

                    match desired with
                    | "System" -> this.RequestedThemeVariant <- null
                    | "Dark" -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Dark
                    | _ -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Light

                let filteredBooks = booksState.Current

                let refreshBooks () =
                    let loaded = loadBooks ()
                    booksState.Set loaded
                    selectedIds.Set []
                    // Show a small debug status of loaded count
                    status.Set(Some(sprintf "Loaded %d from DB" (loaded |> List.length)))

                let addFromPath (file: string) =
                    LibraryService.addBookFromPath file None |> ignore
                    refreshBooks ()

                let refreshAndMarkMissing () =
                    LibraryService.syncMissingFlags () |> ignore
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

                                    ebookType.Patterns <- List<string>([ "*.epub"; "*.pdf"; "*.mobi"; "*.azw3" ])

                                    options.FileTypeFilter <- List<FilePickerFileType>([ ebookType ])

                                    let! storageFiles =
                                        this.StorageProvider.OpenFilePickerAsync(options) |> Async.AwaitTask

                                    let mutable added = 0
                                    let mutable skipped = 0

                                    storageFiles
                                    |> Seq.choose (fun file ->
                                        let path = file.TryGetLocalPath()

                                        if String.IsNullOrWhiteSpace path then None else Some path)
                                    |> Seq.iter (fun p ->
                                        if LibraryService.addBookFromPath p None then
                                            added <- added + 1
                                        else
                                            skipped <- skipped + 1)

                                    // Clear filters so newly added books are visible
                                    query.Set ""
                                    refreshAndMarkMissing ()
                                    // Extra refresh to ensure UI state updates after DB ops
                                    booksState.Set(loadBooks ())
                                    status.Set(Some(sprintf "Added %d, skipped %d" added skipped))
                                }
                                |> Async.StartImmediate)
                          // Cycle System -> Light -> Dark (see handler below)
                          OnImportFolder =
                            (fun () ->
                                async {
                                    let options = FolderPickerOpenOptions()
                                    options.AllowMultiple <- false

                                    let! storageFolders =
                                        this.StorageProvider.OpenFolderPickerAsync(options) |> Async.AwaitTask

                                    match storageFolders |> Seq.tryHead with
                                    | None -> ()
                                    | Some folder ->
                                        let itemsEnum = folder.GetItemsAsync()
                                        let mutable acc = []
                                        let e = itemsEnum.GetAsyncEnumerator()
                                        let mutable hasNext = true

                                        while hasNext do
                                            let! moved = e.MoveNextAsync().AsTask() |> Async.AwaitTask

                                            if moved then acc <- e.Current :: acc else hasNext <- false

                                        do! e.DisposeAsync().AsTask() |> Async.AwaitTask

                                        let rec isBookFile (path: string) =
                                            let ext = System.IO.Path.GetExtension(path).ToLowerInvariant()

                                            [ ".epub"; ".pdf"; ".mobi"; ".azw3" ] |> List.contains ext

                                        let rec enumerate (folder: IStorageFolder) =
                                            task {
                                                let items = folder.GetItemsAsync()
                                                let e = items.GetAsyncEnumerator()
                                                let mutable results: string list = []
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
                                                    else
                                                        hasNext <- false

                                                do! e.DisposeAsync().AsTask()
                                                return results
                                            }

                                        let! files = enumerate folder |> Async.AwaitTask

                                        let mutable added = 0
                                        let mutable skipped = 0

                                        files
                                        |> List.iter (fun p ->
                                            if LibraryService.addBookFromPath p None then
                                                added <- added + 1
                                            else
                                                skipped <- skipped + 1)

                                        // Clear filters so newly imported books are visible
                                        query.Set ""
                                        refreshAndMarkMissing ()
                                        // Extra refresh to ensure UI state updates after DB ops
                                        booksState.Set(loadBooks ())
                                        status.Set(Some(sprintf "Added %d, skipped %d" added skipped))
                                }
                                |> Async.StartImmediate)
                          Theme = theme.Current
                          OnThemeToggle =
                            (fun () ->
                                let next =
                                    match theme.Current with
                                    | "System" -> "Light"
                                    | "Light" -> "Dark"
                                    | _ -> "System"

                                theme.Set next
                                ConfigService.setTheme (Some next)

                                match next with
                                | "System" -> this.RequestedThemeVariant <- null
                                | "Dark" -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Dark
                                | _ -> this.RequestedThemeVariant <- Avalonia.Styling.ThemeVariant.Light)
                          Status = status.Current }
                // Keyboard delete removes selected books
                this.KeyDown.Add(fun e ->
                    if e.Key = Avalonia.Input.Key.Delete && not (List.isEmpty selectedIds.Current) then
                        LibraryService.deleteBooks selectedIds.Current
                        refreshBooks ())

                let selectedBook =
                    lazy
                        (match selectedIds.Current with
                         | id :: _ -> booksState.Current |> List.tryFind (fun b -> b.Id = id)
                         | [] -> None)

                // Initialize columns from config or default
                let initialColumns: Excalibur.Views.BookGrid.ColumnConfig list =
                    match ConfigService.getColumns () with
                    | Some ids when ids.Length > 0 ->
                        let all =
                            [ { Id = ColumnId.Title
                                Header = "Title" }
                              { Id = ColumnId.Author
                                Header = "Author" }
                              { Id = ColumnId.ColTags
                                Header = "Tags" }
                              { Id = ColumnId.Added
                                Header = "Added" }
                              { Id = ColumnId.Missing
                                Header = "Missing" } ]

                        ids
                        |> List.choose (fun name ->
                            match name with
                            | "Title" -> Some(List.head all)
                            | "Author" -> Some all[1]
                            | "Tags" -> Some all[2]
                            | "Added" -> Some all[3]
                            | "Missing" -> Some all[4]
                            | _ -> None)
                    | _ ->
                        [ { Id = ColumnId.Title
                            Header = "Title" }
                          { Id = ColumnId.Author
                            Header = "Author" }
                          { Id = ColumnId.ColTags
                            Header = "Tags" }
                          { Id = ColumnId.Added
                            Header = "Added" }
                          { Id = ColumnId.Missing
                            Header = "Missing" } ]

                let columnsState = ctx.useState initialColumns

                // Persist columns whenever changed
                do columnsState.Current |> List.map (fun c -> c.Header) |> ConfigService.setColumns

                let sortKeyState = ctx.useState "Title"
                let sortAscState = ctx.useState true
                let missingOnly = ctx.useState false
                let authorSearch = ctx.useState ""
                let seriesSearch = ctx.useState ""
                let tagSearch = ctx.useState ""

                let reload () =
                    let filters: Excalibur.Data.LibraryRepository.Filters =
                        { TitleContains =
                            if String.IsNullOrWhiteSpace query.Current then
                                None
                            else
                                Some query.Current
                          AuthorContains = None
                          TagContains = None
                          MissingOnly = if missingOnly.Current then Some true else None
                          AuthorEquals = None
                          TagEquals = None
                          SeriesEquals = None }

                    let sortKey =
                        match sortKeyState.Current with
                        | "Title" -> Excalibur.Data.LibraryRepository.SortKey.Title
                        | "Author" -> Excalibur.Data.LibraryRepository.SortKey.Author
                        | "Added" -> Excalibur.Data.LibraryRepository.SortKey.AddedAt
                        | "Missing" -> Excalibur.Data.LibraryRepository.SortKey.Missing
                        | _ -> Excalibur.Data.LibraryRepository.SortKey.Title

                    let sorts: Excalibur.Data.LibraryRepository.SortSpec list =
                        [ { Key = sortKey
                            Asc = sortAscState.Current } ]

                    let page: Excalibur.Data.LibraryRepository.Page option =
                        Some
                            { Offset = pageIndex.Current * pageSize
                              Limit = pageSize }

                    let total = LibraryService.countBooks filters
                    let items = LibraryService.queryBooks filters sorts page
                    totalCount.Set total
                    booksState.Set items
                    selectedIds.Set []

                let bookListView: Types.IView =
                    // Use TreeDataGrid-based BookGrid
                    BookGrid.view
                        { Books = filteredBooks
                          Columns = columnsState.Current
                          OnColumnsChanged =
                            (fun cols ->
                                // Prevent empty UI: ensure at least one column remains
                                let safe = if cols.IsEmpty then columnsState.Current else cols

                                columnsState.Set safe
                                // Persist happens via the do-binding above
                                ())
                          OnSelectionChanged = (fun ids -> selectedIds.Set ids)
                          OnSort =
                            (fun (key, asc) ->
                                if
                                    not (String.Equals(key, sortKeyState.Current, StringComparison.OrdinalIgnoreCase))
                                then
                                    sortKeyState.Set key

                                if asc <> sortAscState.Current then
                                    sortAscState.Set asc

                                pageIndex.Set 0
                                reload ())
                          SortKey = sortKeyState.Current
                          SortAsc = sortAscState.Current }

                let editBar = EditBar.view { OnBatchEdit = handleBatchEdit }

                let authorFacetItems =
                    let value = authorSearch.Current.Trim()
                    let baseList = AuthorsRepository.listWithCounts ()

                    (if String.IsNullOrWhiteSpace value then
                         baseList
                     else
                         baseList
                         |> List.filter (fun (_, name, _, _) ->
                             name.ToLowerInvariant().Contains(value.ToLowerInvariant())))
                    |> List.map (fun (_, name, _, count) -> ({ Name = name; Count = count }: FacetItem))

                let seriesFacetItems =
                    let value = seriesSearch.Current.Trim()

                    (if String.IsNullOrWhiteSpace value then
                         SeriesRepository.listWithCounts ()
                     else
                         SeriesRepository.listWithCountsLike value)
                    |> List.map (fun (_, name, count) -> ({ Name = name; Count = count }: FacetItem))

                let tagFacetItems =
                    let value = tagSearch.Current.Trim()

                    (if String.IsNullOrWhiteSpace value then
                         TagsRepository.listWithCounts ()
                     else
                         TagsRepository.listWithCountsLike value)
                    |> List.map (fun (_, name, count) -> ({ Name = name; Count = count }: FacetItem))

                let tagBrowser =
                    TagBrowser.view
                        { Authors = authorFacetItems
                          Series = seriesFacetItems
                          Tags = tagFacetItems
                          AuthorSearch = authorSearch.Current
                          OnAuthorSearchChanged = authorSearch.Set
                          SeriesSearch = seriesSearch.Current
                          OnSeriesSearchChanged = seriesSearch.Set
                          TagSearch = tagSearch.Current
                          OnTagSearchChanged = tagSearch.Set
                          OnFilterByTag =
                            (fun tagOpt ->
                                tagEq.Set tagOpt
                                query.Set ""
                                pageIndex.Set 0
                                reload ())
                          OnFilterByAuthor =
                            (fun authorOpt ->
                                authorEq.Set authorOpt
                                query.Set ""
                                pageIndex.Set 0
                                reload ())
                          OnFilterBySeries =
                            (fun seriesOpt ->
                                seriesEq.Set seriesOpt
                                query.Set ""
                                pageIndex.Set 0
                                reload ()) }

                let detailsPane = BookDetails.view { Selected = selectedBook.Value }

                let threePane =
                    Grid.create
                        [ Grid.rowDefinitions "Auto,*"
                          Grid.columnDefinitions "240,*,320"
                          Grid.children
                              [ Grid.create [ Grid.row 0; Grid.columnSpan 3; Grid.children [ topBar ] ]
                                Grid.create
                                    [ Grid.row 1
                                      Grid.column 0
                                      Grid.children [ ScrollViewer.create [ ScrollViewer.content tagBrowser ] ] ]
                                Grid.create
                                    [ Grid.row 1
                                      Grid.column 1
                                      Grid.rowDefinitions "Auto,*"
                                      Grid.children
                                          [ StackPanel.create
                                                [ Grid.row 0
                                                  StackPanel.orientation Orientation.Horizontal
                                                  StackPanel.spacing 8.0
                                                  StackPanel.children
                                                      [ TextBlock.create
                                                            [ TextBlock.text (
                                                                  sprintf
                                                                      "Books: %d / %d"
                                                                      (filteredBooks |> List.length)
                                                                      totalCount.Current
                                                              ) ]
                                                        ToggleSwitch.create
                                                            [ ToggleSwitch.content "Missing only"
                                                              ToggleSwitch.isChecked missingOnly.Current
                                                              ToggleSwitch.onChecked (fun _ ->
                                                                  missingOnly.Set(not missingOnly.Current)
                                                                  pageIndex.Set 0
                                                                  reload ()) ]
                                                        Button.create
                                                            [ Button.content "Reset Columns"
                                                              Button.onClick (fun _ ->
                                                                  let defaults
                                                                      : Excalibur.Views.BookGrid.ColumnConfig list =
                                                                      [ { Id = ColumnId.Title
                                                                          Header = "Title" }
                                                                        { Id = ColumnId.Author
                                                                          Header = "Author" }
                                                                        { Id = ColumnId.ColTags
                                                                          Header = "Tags" }
                                                                        { Id = ColumnId.Added
                                                                          Header = "Added" }
                                                                        { Id = ColumnId.Missing
                                                                          Header = "Missing" } ]

                                                                  columnsState.Set defaults

                                                                  ConfigService.setColumns (
                                                                      defaults |> List.map (fun c -> c.Header)
                                                                  )) ] ] ]
                                            Border.create
                                                [ Grid.row 1
                                                  Border.borderThickness (Avalonia.Thickness 0.0)
                                                  Border.borderBrush (Avalonia.Media.Brushes.Red)
                                                  Border.horizontalAlignment
                                                      Avalonia.Layout.HorizontalAlignment.Stretch
                                                  Border.verticalAlignment Avalonia.Layout.VerticalAlignment.Stretch
                                                  // Restore TreeDataGrid BookGrid as the content
                                                  Border.child bookListView ] ] ]
                                StackPanel.create
                                    [ Grid.row 1
                                      Grid.column 1
                                      StackPanel.orientation Orientation.Horizontal
                                      StackPanel.horizontalAlignment HorizontalAlignment.Center
                                      StackPanel.spacing 8.0
                                      StackPanel.children
                                          [ Button.create
                                                [ Button.content "Prev"
                                                  Button.isEnabled (pageIndex.Current > 0)
                                                  Button.onClick (fun _ ->
                                                      pageIndex.Set(Math.Max(0, pageIndex.Current - 1))
                                                      reload ()) ]
                                            TextBlock.create
                                                [ TextBlock.text (sprintf "Page %d" (pageIndex.Current + 1)) ]
                                            Button.create
                                                [ Button.content "Next"
                                                  Button.isEnabled (
                                                      (pageIndex.Current + 1) * pageSize < totalCount.Current
                                                  )
                                                  Button.onClick (fun _ ->
                                                      pageIndex.Set(pageIndex.Current + 1)
                                                      reload ()) ] ] ]
                                Grid.create
                                    [ Grid.row 1
                                      Grid.column 2
                                      Grid.children [ Border.create [ Border.child detailsPane ] ] ] ] ]

                DockPanel.create [ DockPanel.children [ threePane; editBar ] ])
