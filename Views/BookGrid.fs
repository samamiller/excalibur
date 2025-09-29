module Excalibur.Views.BookGrid

open System
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
      OnSort: string * bool -> unit }

let private defaultColumns =
    [ { Id = Title; Header = "Title" }
      { Id = Author; Header = "Author" }
      { Id = ColTags; Header = "Tags" }
      { Id = Added; Header = "Added" }
      { Id = Missing; Header = "Missing" } ]

let private cell (text: string) =
    TextBlock.create [ TextBlock.text text ] :> Types.IView

[<CompiledName("TitleText")>]
let private titleText (book: Book) = book.title

[<CompiledName("AuthorText")>]
let private authorText (book: Book) = defaultArg book.author ""

[<CompiledName("TagsText")>]
let private tagsText (book: Book) = defaultArg book.tags ""

[<CompiledName("AddedAtText")>]
let private addedAtText (book: Book) = defaultArg book.added_at ""

[<CompiledName("MissingText")>]
let private missingText (book: Book) = if book.missing then "Yes" else ""

// Static helpers for expression-based TextColumns (use a real class for stable MethodInfo)
type ColumnHelpers =
    static member TitleOf(b: Book) = b.title
    static member AuthorOf(b: Book) = defaultArg b.author ""
    static member TagsOf(b: Book) = defaultArg b.tags ""
    static member AddedOf(b: Book) = defaultArg b.added_at ""
    static member MissingOf(b: Book) = if b.missing then "Yes" else ""

let private colTemplate (cfg: ColumnConfig) : Types.IView =
    match cfg.Id with
    | Title ->
        DataGridTemplateColumn.create [ DataGridTemplateColumn.header cfg.Header
                                        DataGridTemplateColumn.cellTemplate (
                                            DataTemplateView<Book>.create (fun b -> cell b.title)
                                        ) ]
    | Author ->
        DataGridTemplateColumn.create [ DataGridTemplateColumn.header cfg.Header
                                        DataGridTemplateColumn.cellTemplate (
                                            DataTemplateView<Book>.create (fun b -> cell (defaultArg b.author ""))
                                        ) ]
    | ColTags ->
        DataGridTemplateColumn.create [ DataGridTemplateColumn.header cfg.Header
                                        DataGridTemplateColumn.cellTemplate (
                                            DataTemplateView<Book>.create (fun b -> cell (defaultArg b.tags ""))
                                        ) ]
    | Added ->
        DataGridTemplateColumn.create [ DataGridTemplateColumn.header cfg.Header
                                        DataGridTemplateColumn.cellTemplate (
                                            DataTemplateView<Book>.create (fun b -> cell (defaultArg b.added_at ""))
                                        ) ]
    | Missing ->
        DataGridTemplateColumn.create [ DataGridTemplateColumn.header cfg.Header
                                        DataGridTemplateColumn.cellTemplate (
                                            DataTemplateView<Book>.create
                                                (fun b -> cell (if b.missing then "Yes" else ""))
                                        ) ]

let view (props: Props) : Types.IView =
    let cols =
        if props.Columns.IsEmpty then
            defaultColumns
        else
            props.Columns

    do
        Logger.infof "BookGrid: books=%d requestedColumns=%d" props.Books.Length props.Columns.Length

        Logger.infof
            "BookGrid: effectiveColumns=%d [%s]"
            cols.Length
            (String.Join(", ", (cols |> List.map (fun c -> c.Header))))

    // TreeDataGrid works with a Source object; we’ll create a flat hierarchical source
    // with no children (single-level tree) to mirror existing list behavior.
    let items = props.Books |> List.toArray

    let makeColumn header (valueExpr: ParameterExpression -> Expression) (valueFn: Book -> string) : IColumn<Book> =
        let parameter = Expression.Parameter(typeof<Book>, "b")
        let body = valueExpr parameter
        let lambda = Expression.Lambda<Func<Book, string>>(body, parameter)
        let options = TextColumnOptions<Book>()
        options.CanUserSortColumn <- Nullable true
        let comparer = StringComparer.OrdinalIgnoreCase
        options.CompareAscending <- Comparison<Book>(fun a b -> comparer.Compare(valueFn a, valueFn b))
        options.CompareDescending <- Comparison<Book>(fun a b -> comparer.Compare(valueFn b, valueFn a))
        TextColumn<Book, string>(header, lambda, Nullable<GridLength>(), options) :> IColumn<Book>

    let moduleType = typeof<ColumnId>.DeclaringType

    let method name =
        moduleType.GetMethod(name, BindingFlags.Static ||| BindingFlags.NonPublic)

    // Text columns using LINQ expressions for rendering; styles are provided by merged TreeDataGrid theme
    let titleColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod ("TitleOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        TextColumn<Book, string>("Title", lambda) :> IColumn<Book>

    let authorColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod ("AuthorOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        TextColumn<Book, string>("Author", lambda) :> IColumn<Book>

    let tagsColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod ("TagsOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        TextColumn<Book, string>("Tags", lambda) :> IColumn<Book>

    let addedColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod ("AddedOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        TextColumn<Book, string>("Added", lambda) :> IColumn<Book>

    let missingColumn: IColumn<Book> =
        let mi =
            typeof<ColumnHelpers>.GetMethod ("MissingOf", BindingFlags.Public ||| BindingFlags.Static)

        let p = Expression.Parameter(typeof<Book>, "b")
        let body = Expression.Call(mi, p)
        let lambda = Expression.Lambda<Func<Book, string>>(body, p)
        TextColumn<Book, string>("Missing", lambda) :> IColumn<Book>

    // Flat source for single-level list; no expander column needed
    let source: ITreeDataGridSource =
        let s = new Avalonia.Controls.FlatTreeDataGridSource<Book>(items)

        let addById (id: ColumnId) =
            match id with
            | Title -> s.Columns.Add(titleColumn)
            | Author -> s.Columns.Add(authorColumn)
            | ColTags -> s.Columns.Add(tagsColumn)
            | Added -> s.Columns.Add(addedColumn)
            | Missing -> s.Columns.Add(missingColumn)

        let before = s.Columns.Count
        cols |> List.iter (fun c -> addById c.Id)
        let after = s.Columns.Count
        Logger.infof "BookGrid: addedColumns=%d totalColumns=%d" (after - before) after
        s :> ITreeDataGridSource

    // Simple column chooser context menu (toggle visibility)
    let columnMenu: ContextMenu =
        let mk (cfg: ColumnConfig) =
            let isOn = cols |> List.exists (fun c -> c.Id = cfg.Id)
            let item = new MenuItem()
            item.Header <- cfg.Header
            item.IsChecked <- isOn

            item.Click.Add (fun _ ->
                let next =
                    if isOn then
                        cols |> List.filter (fun c -> c.Id <> cfg.Id)
                    else
                        cols @ [ cfg ]

                props.OnColumnsChanged next)

            item :> obj

        let cm = new ContextMenu()

        cm.ItemsSource <-
            [ mk { Id = Title; Header = "Title" }
              mk { Id = Author; Header = "Author" }
              mk { Id = ColTags; Header = "Tags" }
              mk { Id = Added; Header = "Added" }
              mk { Id = Missing; Header = "Missing" } ]

        cm

    // TreeDataGrid selection reporting: project selected rows to IDs by reading the flat source
    let onSelectionChanged (tdg: Avalonia.Controls.TreeDataGrid) =
        match tdg.Source with
        | :? Avalonia.Controls.FlatTreeDataGridSource<Book> as s when not (isNull s.RowSelection) ->
            s.RowSelection.SelectedItems
            |> Seq.map (fun b -> b.id)
            |> Seq.toList
            |> props.OnSelectionChanged
        | _ -> ()

    TreeDataGrid.create [ TreeDataGrid.source source
                          TreeDataGrid.showColumnHeaders true
                          // Attach context menu for column chooser
                          Control.contextMenu columnMenu ]

// Minimal sort support: handle AutoGeneratingColumn not available; rely on DataGrid's own sort toggling.
// For now, we expose a helper to attach a handler externally if needed.
