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

    let titleColumn =
        let mi = method "TitleText"
        makeColumn "Title" (fun param -> Expression.Call(mi, param)) titleText

    let authorColumn =
        let mi = method "AuthorText"
        makeColumn "Author" (fun param -> Expression.Call(mi, param)) authorText

    let tagsColumn =
        let mi = method "TagsText"
        makeColumn "Tags" (fun param -> Expression.Call(mi, param)) tagsText

    let addedColumn =
        let mi = method "AddedAtText"
        makeColumn "Added" (fun param -> Expression.Call(mi, param)) addedAtText

    let missingColumn =
        let mi = method "MissingText"
        makeColumn "Missing" (fun param -> Expression.Call(mi, param)) missingText

    // Hierarchical source (single level for now) prepares us for grouping later
    let source: ITreeDataGridSource =
        let s = new HierarchicalTreeDataGridSource<Book>(items :> seq<Book>)

        let addById (id: ColumnId) =
            match id with
            | Title -> s.Columns.Add(titleColumn)
            | Author -> s.Columns.Add(authorColumn)
            | ColTags -> s.Columns.Add(tagsColumn)
            | Added -> s.Columns.Add(addedColumn)
            | Missing -> s.Columns.Add(missingColumn)

        cols |> List.iter (fun c -> addById c.Id)
        s :> ITreeDataGridSource

    // TreeDataGrid selection reporting: we'll observe SelectedRows on the control when selection changes.
    TreeDataGrid.create [ TreeDataGrid.source source
                          TreeDataGrid.showColumnHeaders true ]

// Minimal sort support: handle AutoGeneratingColumn not available; rely on DataGrid's own sort toggling.
// For now, we expose a helper to attach a handler externally if needed.
