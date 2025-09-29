module Excalibur.Views.BookGrid

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Excalibur.Domain
open Avalonia.Data
open Avalonia.Interactivity
open Avalonia.Controls.Models.TreeDataGrid
open System.Linq.Expressions
open System.Reflection
open Excalibur.Services

// Use our FuncUI DSL wrapper for TreeDataGrid
open Avalonia.FuncUI.DSL.TreeDataGrid
open Avalonia.Input
open Avalonia.Controls.Primitives
open System.Collections.Generic
open Avalonia.VisualTree
open System.ComponentModel

type ColumnId =
    | Title
    | Author
    | ColTags
    | Added
    | Missing

type ColumnConfig = { Id: ColumnId; Header: string }

type Props =
    { Books: Book list
      Columns: ColumnConfig list
      OnColumnsChanged: ColumnConfig list -> unit
      OnSelectionChanged: int list -> unit
      OnSort: string * bool -> unit
      SortKey: string
      SortAsc: bool }

let private defaultColumns =
    [ { Id = Title; Header = "Title" }
      { Id = Author; Header = "Author" }
      { Id = ColTags; Header = "Tags" }
      { Id = Added; Header = "Added" }
      { Id = Missing; Header = "Missing" } ]

let private pointerTraceEnabled () =
    match Environment.GetEnvironmentVariable "EXCALIBUR_TDG_TRACE" with
    | null
    | "" -> false
    | value when value.Equals("1", StringComparison.OrdinalIgnoreCase) -> true
    | value when value.Equals("true", StringComparison.OrdinalIgnoreCase) -> true
    | _ -> false

let private describeData (data: obj) : string option =
    match data with
    | null -> None
    | :? Book as b -> Some(sprintf "Book(%d) \"%s\"" b.Id b.Title)
    | other -> Some(other.GetType().Name)

let private describeControl (ctrl: Control) : string =
    let typeName = ctrl.GetType().Name

    let namePart =
        if String.IsNullOrWhiteSpace ctrl.Name then
            ""
        else
            sprintf " #%s" ctrl.Name

    let dataPart =
        match describeData ctrl.DataContext with
        | Some info -> sprintf " Data=%s" info
        | None -> ""

    typeName + namePart + dataPart

let private controlChain (ctrl: Control) (maxDepth: int) : Control list =
    let rec collect (node: Control) depth acc =
        if isNull (box node) || depth > maxDepth then
            List.rev acc
        else
            let acc' = node :: acc

            match node.Parent with
            | :? Control as parent -> collect parent (depth + 1) acc'
            | _ -> List.rev acc'

    collect ctrl 0 []

let private findHeader (ctrl: Control) : TreeDataGridColumnHeader option =
    controlChain ctrl 10
    |> List.tryPick (function
        | :? TreeDataGridColumnHeader as header -> Some header
        | _ -> None)

let private tryTextProperty (value: obj) : string option =
    if isNull value then
        None
    else
        let prop =
            value
                .GetType()
                .GetProperty("Text", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.FlattenHierarchy)

        match prop with
        | null -> None
        | p ->
            match p.GetValue(value) with
            | :? string as s when not (String.IsNullOrWhiteSpace s) -> Some s
            | _ -> None

let private extractHeaderText (value: obj) : string option =
    let rec loop (state: obj) : string option =
        match state with
        | null -> None
        | :? string as s when not (String.IsNullOrWhiteSpace s) -> Some s
        | :? TextBlock as tb when not (String.IsNullOrWhiteSpace tb.Text) -> Some tb.Text
        | :? HeaderedContentControl as hc ->
            match loop hc.Header with
            | Some text -> Some text
            | None -> loop hc.Content
        | :? ContentControl as cc -> loop cc.Content
        | other -> tryTextProperty other

    loop value

let private headerText (header: TreeDataGridColumnHeader) : string option =
    match extractHeaderText header.Content with
    | Some text -> Some text
    | None -> extractHeaderText header.DataContext

let private logPointerRoute (e: PointerPressedEventArgs) =
    let point = e.GetCurrentPoint(null)
    let position = point.Position
    let left = point.Properties.IsLeftButtonPressed
    let right = point.Properties.IsRightButtonPressed
    let middle = point.Properties.IsMiddleButtonPressed

    let sourceName =
        match e.Source with
        | null -> "(null)"
        | src -> src.GetType().FullName

    Logger.infof
        "TDG pointer: handled=%b route=%A left=%b right=%b middle=%b pos=%.1f,%.1f source=%s"
        e.Handled
        e.RoutedEvent.RoutingStrategies
        left
        right
        middle
        position.X
        position.Y
        sourceName

    match e.Source with
    | :? Control as ctrl ->
        let chain = controlChain ctrl 12

        chain
        |> List.indexed
        |> List.iter (fun (idx, node) -> Logger.infof "TDG pointer chain[%d]: %s" idx (describeControl node))

        chain
        |> List.tryPick (function
            | :? TreeDataGrid as tdg -> Some tdg
            | _ -> None)
        |> Option.iter (fun tdg ->
            let sourceInfo =
                if isNull tdg.Source then
                    "source=(null)"
                else
                    sprintf "source=%s" (tdg.Source.GetType().Name)

            let selectionInfo =
                match tdg.Source with
                | :? Avalonia.Controls.FlatTreeDataGridSource<Book> as s when not (isNull s.RowSelection) ->
                    sprintf "rowSelection items=%d" (s.RowSelection.SelectedItems |> Seq.length)
                | _ -> "rowSelection unavailable"

            Logger.infof "TDG pointer tree: %s %s" sourceInfo selectionInfo)
    | _ -> ()

let private cell (text: string) =
    TextBlock.create [ TextBlock.text text ] :> Types.IView

[<CompiledName("TitleText")>]
let private titleText (book: Book) = book.Title

[<CompiledName("AuthorText")>]
let private authorText (book: Book) = defaultArg book.Author ""

[<CompiledName("TagsText")>]
let private tagsText (book: Book) = defaultArg book.Tags ""

[<CompiledName("AddedAtText")>]
let private addedAtText (book: Book) = defaultArg book.AddedAt ""

[<CompiledName("MissingText")>]
let private missingText (book: Book) = if book.Missing then "Yes" else ""

// Static helpers for expression-based TextColumns (use a real class for stable MethodInfo)
type ColumnHelpers =
    static member TitleOf(b: Book) = b.Title
    static member AuthorOf(b: Book) = defaultArg b.Author ""
    static member TagsOf(b: Book) = defaultArg b.Tags ""
    static member AddedOf(b: Book) = defaultArg b.AddedAt ""
    static member MissingOf(b: Book) = if b.Missing then "Yes" else ""

let view (props: Props) : Types.IView =
    let cols =
        if props.Columns.IsEmpty then
            defaultColumns
        else
            props.Columns

    // TreeDataGrid works with a Source object; we’ll create a flat hierarchical source
    // with no children (single-level tree) to mirror existing list behavior.
    // Sort state comes from parent (explicit UI controls)
    let sortKey = props.SortKey
    let sortAsc = props.SortAsc

    let sortItems (arr: Book array) =
        let cmp = StringComparer.OrdinalIgnoreCase

        let keyOf (b: Book) =
            match sortKey with
            | "Title" -> b.Title
            | "Author" -> defaultArg b.Author ""
            | "Tags" -> defaultArg b.Tags ""
            | "Added" -> defaultArg b.AddedAt ""
            | "Missing" -> if b.Missing then "Yes" else ""
            | _ -> b.Title

        let sorted = arr |> Array.sortWith (fun a b -> cmp.Compare(keyOf a, keyOf b))

        if sortAsc then sorted else sorted |> Array.rev

    let items = props.Books |> List.toArray |> sortItems

    let indexById =
        let dict = Dictionary<int, int>()

        items |> Array.iteri (fun idx book -> dict[book.Id] <- idx)

        dict

    // Text columns using LINQ expressions for rendering; styles are provided by merged TreeDataGrid theme
    let setSortDirection (header: string) (col: TextColumn<Book, string>) =
        if header.Equals(props.SortKey, StringComparison.OrdinalIgnoreCase) then
            let dir =
                if props.SortAsc then
                    ListSortDirection.Ascending
                else
                    ListSortDirection.Descending

            col.SortDirection <- Nullable dir
        else
            col.SortDirection <- Nullable()

        col

    let titleColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod("TitleOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        let opts = TextColumnOptions<Book>()
        opts.CanUserSortColumn <- Nullable true
        let cmp = StringComparer.OrdinalIgnoreCase
        opts.CompareAscending <- Comparison<Book>(fun a b -> cmp.Compare(a.Title, b.Title))
        opts.CompareDescending <- Comparison<Book>(fun a b -> cmp.Compare(b.Title, a.Title))
        let col = TextColumn<Book, string>("Title", lambda, Nullable(), opts)
        setSortDirection "Title" col :> IColumn<Book>

    let authorColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod("AuthorOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        let opts = TextColumnOptions<Book>()
        opts.CanUserSortColumn <- Nullable true
        let cmp = StringComparer.OrdinalIgnoreCase

        opts.CompareAscending <-
            Comparison<Book>(fun a b -> cmp.Compare((defaultArg a.Author ""), (defaultArg b.Author "")))

        opts.CompareDescending <-
            Comparison<Book>(fun a b -> cmp.Compare((defaultArg b.Author ""), (defaultArg a.Author "")))

        let col = TextColumn<Book, string>("Author", lambda, Nullable(), opts)
        setSortDirection "Author" col :> IColumn<Book>

    let tagsColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod("TagsOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        let opts = TextColumnOptions<Book>()
        opts.CanUserSortColumn <- Nullable true
        let cmp = StringComparer.OrdinalIgnoreCase

        opts.CompareAscending <-
            Comparison<Book>(fun a b -> cmp.Compare((defaultArg a.Tags ""), (defaultArg b.Tags "")))

        opts.CompareDescending <-
            Comparison<Book>(fun a b -> cmp.Compare((defaultArg b.Tags ""), (defaultArg a.Tags "")))

        let col = TextColumn<Book, string>("Tags", lambda, Nullable(), opts)
        setSortDirection "Tags" col :> IColumn<Book>

    let addedColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod("AddedOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        let opts = TextColumnOptions<Book>()
        opts.CanUserSortColumn <- Nullable true
        let cmp = StringComparer.OrdinalIgnoreCase

        opts.CompareAscending <-
            Comparison<Book>(fun a b -> cmp.Compare((defaultArg a.AddedAt ""), (defaultArg b.AddedAt "")))

        opts.CompareDescending <-
            Comparison<Book>(fun a b -> cmp.Compare((defaultArg b.AddedAt ""), (defaultArg a.AddedAt "")))

        let col = TextColumn<Book, string>("Added", lambda, Nullable(), opts)
        setSortDirection "Added" col :> IColumn<Book>

    let missingColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod("MissingOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        let opts = TextColumnOptions<Book>()
        opts.CanUserSortColumn <- Nullable true
        let cmp = StringComparer.OrdinalIgnoreCase
        let miss (b: Book) = if b.Missing then "Yes" else ""
        opts.CompareAscending <- Comparison<Book>(fun a b -> cmp.Compare(miss a, miss b))
        opts.CompareDescending <- Comparison<Book>(fun a b -> cmp.Compare(miss b, miss a))
        let col = TextColumn<Book, string>("Missing", lambda, Nullable(), opts)
        setSortDirection "Missing" col :> IColumn<Book>

    // Flat source for single-level list; no expander column needed
    let createSource (data: Book array) : Avalonia.Controls.FlatTreeDataGridSource<Book> =
        let s = new Avalonia.Controls.FlatTreeDataGridSource<Book>(data)

        if not (isNull s.RowSelection) then
            s.RowSelection.SingleSelect <- true

        let addById (id: ColumnId) =
            match id with
            | Title -> s.Columns.Add(titleColumn)
            | Author -> s.Columns.Add(authorColumn)
            | ColTags -> s.Columns.Add(tagsColumn)
            | Added -> s.Columns.Add(addedColumn)
            | Missing -> s.Columns.Add(missingColumn)

        cols |> List.iter (fun c -> addById c.Id)
        s

    let source: ITreeDataGridSource = createSource items :> ITreeDataGridSource

    // Simple column chooser context menu (toggle visibility) and row actions
    let columnMenu: ContextMenu =
        let mk (cfg: ColumnConfig) =
            let isOn = cols |> List.exists (fun c -> c.Id = cfg.Id)
            let item = MenuItem()
            item.Header <- cfg.Header
            item.IsChecked <- isOn

            item.Click.Add(fun _ ->
                let next =
                    if isOn then
                        cols |> List.filter (fun c -> c.Id <> cfg.Id)
                    else
                        cols @ [ cfg ]

                props.OnColumnsChanged next)

            item :> obj

        let cm = ContextMenu()

        // Column toggles
        let columnItems: obj list =
            [ mk { Id = Title; Header = "Title" }
              mk { Id = Author; Header = "Author" }
              mk { Id = ColTags; Header = "Tags" }
              mk { Id = Added; Header = "Added" }
              mk { Id = Missing; Header = "Missing" } ]

        // Row actions (applied to current row when right-clicked)
        let openItem = MenuItem()
        openItem.Header <- "Open File"

        openItem.Click.Add(fun _ ->
            let target = cm.PlacementTarget

            if not (isNull (box target)) then
                let ctrl: Control = target
                // Climb up to find the bound Book for the row
                let rec findBook (c: Control) depth =
                    if isNull (box c) || depth > 10 then
                        None
                    else
                        match c.DataContext with
                        | :? Book as b -> Some b
                        | _ ->
                            match c.Parent with
                            | :? Control as p -> findBook p (depth + 1)
                            | _ -> None

                match findBook ctrl 0 with
                | Some b when not b.Missing && not (String.IsNullOrWhiteSpace b.Path) ->
                    try
                        let psi = System.Diagnostics.ProcessStartInfo()
                        psi.FileName <- b.Path
                        psi.UseShellExecute <- true
                        System.Diagnostics.Process.Start(psi) |> ignore
                    with ex ->
                        Logger.warnf "Failed to open file '%s': %s" b.Path ex.Message
                | Some b -> Logger.warnf "Cannot open missing file id=%d" b.Id
                | None -> ())

        let editMetaItem = MenuItem()
        editMetaItem.Header <- "Edit Metadata…"

        editMetaItem.Click.Add(fun _ ->
            let target = cm.PlacementTarget

            if not (isNull (box target)) then
                let ctrl: Control = target
                // climb to find the row's book
                let rec findBook (c: Control) depth =
                    if isNull (box c) || depth > 10 then
                        None
                    else
                        match c.DataContext with
                        | :? Book as b -> Some b
                        | _ ->
                            match c.Parent with
                            | :? Control as p -> findBook p (depth + 1)
                            | _ -> None

                // helper to find enclosing grid
                let rec findTdg (c: Control) depth =
                    if isNull (box c) || depth > 10 then
                        None
                    else
                        match c with
                        | :? Avalonia.Controls.TreeDataGrid as t -> Some t
                        | _ ->
                            match c.Parent with
                            | :? Control as p -> findTdg p (depth + 1)
                            | _ -> None

                match findBook ctrl 0 with
                | Some b ->
                    // Show the batch edit dialog to reuse UI, then persist only for this id
                    let w =
                        match TopLevel.GetTopLevel ctrl with
                        | :? Window as win -> win
                        | _ -> null

                    if not (isNull w) then
                        async {
                            let! result = Excalibur.Dialogs.EditMetadataDialog.show w

                            match result with
                            | Some r ->
                                // update only this book
                                Excalibur.Services.LibraryService.updateAuthorTags [ b.Id ] r.Author r.Tags
                                // push current selection back to host if we can reach the grid
                                match findTdg ctrl 0 with
                                | Some tdg ->
                                    match tdg.Source with
                                    | :? Avalonia.Controls.FlatTreeDataGridSource<Book> as s when
                                        not (isNull s.RowSelection)
                                        ->
                                        let selected = s.RowSelection.SelectedItems
                                        selected |> Seq.map (fun b -> b.Id) |> Seq.toList |> props.OnSelectionChanged
                                    | _ -> ()
                                | None -> ()
                            | None -> ()
                        }
                        |> Async.StartImmediate
                | None -> ())

        // Section headers
        let actionsHeader = MenuItem()
        actionsHeader.Header <- "Actions"
        actionsHeader.IsEnabled <- false

        let columnsHeader = MenuItem()
        columnsHeader.Header <- "Columns"
        columnsHeader.IsEnabled <- false

        cm.ItemsSource <-
            (actionsHeader :> obj)
            :: (openItem :> obj)
            :: (editMetaItem :> obj)
            :: (Separator() :> obj)
            :: (columnsHeader :> obj)
            :: columnItems

        cm

    // TreeDataGrid selection reporting: project selected rows to IDs by reading the flat source
    let onSelectionChanged (tdg: Avalonia.Controls.TreeDataGrid) =
        match tdg.Source with
        | :? Avalonia.Controls.FlatTreeDataGridSource<Book> as s when not (isNull s.RowSelection) ->
            let selected = s.RowSelection.SelectedItems
            Logger.infof "TDG selection changed: count=%d" (selected |> Seq.length)

            selected |> Seq.map (fun b -> b.Id) |> Seq.toList |> props.OnSelectionChanged
        | _ -> ()

    // Subscribe to selection changes from the RowSelection model
    let subscribeSelection (ctl: Avalonia.Controls.TreeDataGrid) =
        match ctl.Source with
        | :? Avalonia.Controls.FlatTreeDataGridSource<Book> as s when not (isNull s.RowSelection) ->
            // Push initial
            onSelectionChanged ctl
            // Subscribe
            s.RowSelection.SelectionChanged.Add(fun _ -> onSelectionChanged ctl)
        | _ -> ()

    // Explicit row click ensures selection updates on platforms where default selection is inert
    let onRowPointer (e: PointerPressedEventArgs) : unit =
        if pointerTraceEnabled () then
            logPointerRoute e

        let point = e.GetCurrentPoint(null)
        let isPrimary = point.Properties.IsLeftButtonPressed
        let isSecondary = point.Properties.IsRightButtonPressed

        let rec findTdg (ctrl: Avalonia.Controls.Control) depth =
            if isNull (box ctrl) || depth > 10 then
                None
            else
                match ctrl with
                | :? Avalonia.Controls.TreeDataGrid as t -> Some t
                | _ ->
                    match ctrl.Parent with
                    | :? Avalonia.Controls.Control as parent -> findTdg parent (depth + 1)
                    | _ -> None

        let rec findBook (ctrl: Avalonia.Controls.Control) depth =
            if isNull (box ctrl) || depth > 10 then
                None
            else
                match ctrl.DataContext with
                | :? Book as book -> Some book
                | _ ->
                    match ctrl.Parent with
                    | :? Avalonia.Controls.Control as parent -> findBook parent (depth + 1)
                    | _ -> None

        match e.Source with
        | :? Avalonia.Controls.Control as c ->
            let tdgOpt = findTdg c 0

            let headerCandidateFromSource () =
                let localHit = c.InputHitTest(e.GetPosition(c))

                match localHit with
                | :? TreeDataGridColumnHeader as header -> Some header
                | :? Control as ctrl -> findHeader ctrl
                | _ -> findHeader c

            let headerCandidateViaVisual (tdg: TreeDataGrid) =
                let pointOnGrid = e.GetPosition(tdg)

                VisualExtensions.GetVisualsAt(tdg, pointOnGrid)
                |> Seq.tryPick (function
                    | :? TreeDataGridColumnHeader as header -> Some header
                    | :? Control as ctrl -> findHeader ctrl
                    | _ -> None)

            let headerCandidate =
                if isPrimary then
                    match headerCandidateFromSource () with
                    | Some header -> Some header
                    | None ->
                        match tdgOpt with
                        | Some tdg ->
                            let visuals = headerCandidateViaVisual tdg

                            if pointerTraceEnabled () then
                                Logger.infof "TDG header lookup via visuals: found=%b" (visuals |> Option.isSome)

                            visuals
                        | None -> None
                else
                    None

            match headerCandidate with
            | Some header when isPrimary ->
                if pointerTraceEnabled () then
                    let contentType =
                        if isNull header.Content then
                            "(null)"
                        else
                            header.Content.GetType().Name

                    let dataContextType =
                        if isNull header.DataContext then
                            "(null)"
                        else
                            header.DataContext.GetType().Name

                    Logger.infof
                        "TDG header candidate: %s content=%s dataContext=%s index=%d"
                        (header.GetType().Name)
                        contentType
                        dataContextType
                        header.ColumnIndex

                match headerText header with
                | Some key when not (String.IsNullOrWhiteSpace key) ->
                    let normalizedKey = key.Trim()

                    let resolvedKeyFromIndex =
                        if header.ColumnIndex >= 0 then
                            cols |> List.tryItem header.ColumnIndex |> Option.map (fun c -> c.Header)
                        else
                            None

                    let resolvedKey =
                        resolvedKeyFromIndex
                        |> Option.orElseWith (fun () ->
                            cols
                            |> List.tryFind (fun c ->
                                String.Equals(c.Header, normalizedKey, StringComparison.OrdinalIgnoreCase))
                            |> Option.map (fun c -> c.Header))
                        |> Option.defaultValue normalizedKey

                    let nextAsc =
                        if resolvedKey.Equals(props.SortKey, StringComparison.OrdinalIgnoreCase) then
                            not props.SortAsc
                        else
                            true

                    if pointerTraceEnabled () then
                        Logger.infof "TDG header sort: raw=%s resolved=%s asc=%b" normalizedKey resolvedKey nextAsc

                    props.OnSort(resolvedKey, nextAsc)
                    e.Handled <- true
                | _ ->
                    if header.ColumnIndex >= 0 then
                        match cols |> List.tryItem header.ColumnIndex with
                        | Some cfg ->
                            let resolvedKey = cfg.Header

                            let nextAsc =
                                if resolvedKey.Equals(props.SortKey, StringComparison.OrdinalIgnoreCase) then
                                    not props.SortAsc
                                else
                                    true

                            if pointerTraceEnabled () then
                                Logger.infof "TDG header sort (index fallback): resolved=%s asc=%b" resolvedKey nextAsc

                            props.OnSort(resolvedKey, nextAsc)
                            e.Handled <- true
                        | None -> ()
                    else
                        ()
            | _ when isPrimary || isSecondary ->
                if pointerTraceEnabled () && headerCandidate.IsNone then
                    Logger.infof "TDG header candidate: none"

                match findBook c 0, tdgOpt with
                | Some book, Some tdg ->
                    match tdg.Source with
                    | :? Avalonia.Controls.FlatTreeDataGridSource<Book> as s when not (isNull s.RowSelection) ->
                        match indexById.TryGetValue book.Id with
                        | true, idx ->
                            s.RowSelection.Select(idx)
                            Logger.infof "TDG manual select: id=%d idx=%d" book.Id idx
                            onSelectionChanged tdg
                        | _ ->
                            if s.Rows.Count > 0 then
                                s.RowSelection.Select(0)
                                Logger.infof "TDG manual select fallback idx=0"
                                onSelectionChanged tdg

                        e.Handled <- true
                    | _ -> ()
                | _ -> ()
            | _ -> ()
        | _ -> ()

    TreeDataGrid.create
        [ TreeDataGrid.Source source
          TreeDataGrid.ShowColumnHeaders true
          TreeDataGrid.CanUserSortColumns true
          // Hook selection refresh on attach/source change
          TreeDataGrid.OnSelectionChanged subscribeSelection
          // Row-level pointer to force selection at least once
          TreeDataGrid.OnPointerPressed onRowPointer
          // Attach context menu for column chooser
          Control.contextMenu columnMenu ]

// Minimal sort support: handle AutoGeneratingColumn not available; rely on DataGrid's own sort toggling.
// For now, we expose a helper to attach a handler externally if needed.
