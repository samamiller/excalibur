module Excalibur.Views.BookGrid

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Excalibur.Domain
open Avalonia.Data
open Avalonia.Interactivity

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

let private colTemplate (cfg: ColumnConfig) : Types.IView =
    match cfg.Id with
    | Title ->
        DataGridTemplateColumn.create [
            DataGridTemplateColumn.header cfg.Header
            DataGridTemplateColumn.cellTemplate (DataTemplateView<Book>.create (fun b -> cell b.title))
        ]
    | Author ->
        DataGridTemplateColumn.create [
            DataGridTemplateColumn.header cfg.Header
            DataGridTemplateColumn.cellTemplate (DataTemplateView<Book>.create (fun b -> cell (defaultArg b.author "")))
        ]
    | ColTags ->
        DataGridTemplateColumn.create [
            DataGridTemplateColumn.header cfg.Header
            DataGridTemplateColumn.cellTemplate (DataTemplateView<Book>.create (fun b -> cell (defaultArg b.tags "")))
        ]
    | Added ->
        DataGridTemplateColumn.create [
            DataGridTemplateColumn.header cfg.Header
            DataGridTemplateColumn.cellTemplate (DataTemplateView<Book>.create (fun b -> cell (defaultArg b.added_at "")))
        ]
    | Missing ->
        DataGridTemplateColumn.create [
            DataGridTemplateColumn.header cfg.Header
            DataGridTemplateColumn.cellTemplate (DataTemplateView<Book>.create (fun b -> cell (if b.missing then "Yes" else "")))
        ]

let view (props: Props) : Types.IView =
    let cols = if props.Columns.IsEmpty then defaultColumns else props.Columns

    DataGrid.create [
        DataGrid.isReadOnly true
        // Feed array to avoid any collection type quirks
        DataGrid.items (props.Books |> List.toArray)
        DataGrid.onSelectionChanged (fun args ->
            match args.Source with
            | :? DataGrid as dg ->
                dg.SelectedItems
                |> Seq.cast<Book>
                |> Seq.map (fun b -> b.id)
                |> Seq.toList
                |> props.OnSelectionChanged
            | _ -> () )
        DataGrid.columns (cols |> List.map colTemplate)
    ]

// Minimal sort support: handle AutoGeneratingColumn not available; rely on DataGrid's own sort toggling.
// For now, we expose a helper to attach a handler externally if needed.
