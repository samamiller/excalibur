module Excalibur.Views.BookTable

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Input
open Avalonia
open Excalibur.Domain

type Props =
    { Books: Book list
      OnSelectionChanged: int list -> unit }

let private headerCell text =
    TextBlock.create [ TextBlock.text text
                       TextBlock.fontWeight Avalonia.Media.FontWeight.Bold ]
    :> Types.IView

let private cell text =
    TextBlock.create [ TextBlock.text text ] :> Types.IView

let view (props: Props) : Types.IView =
    let row b : Types.IView =
        Grid.create [ Grid.columnDefinitions "2*,1.2*,1*,140,90"
                      Grid.children [ (cell b.Title) |> Grid.column 0 :> Types.IView
                                      (cell (defaultArg b.Author "")) |> Grid.column 1 :> Types.IView
                                      (cell (defaultArg b.Tags "")) |> Grid.column 2 :> Types.IView
                                      (cell (defaultArg b.AddedAt "")) |> Grid.column 3 :> Types.IView
                                      (cell (if b.Missing then "Yes" else ""))
                                      |> Grid.column 4
                                      :> Types.IView ]
                      Grid.onTapped (fun _ -> props.OnSelectionChanged [ b.Id ]) ]

    let header: Types.IView =
        Grid.create [ Grid.columnDefinitions "2*,1.2*,1*,140,90"
                      Grid.children [ (headerCell "Title") |> Grid.column 0 :> Types.IView
                                      (headerCell "Author") |> Grid.column 1 :> Types.IView
                                      (headerCell "Tags") |> Grid.column 2 :> Types.IView
                                      (headerCell "Added") |> Grid.column 3 :> Types.IView
                                      (headerCell "Missing") |> Grid.column 4 :> Types.IView ] ]

    StackPanel.create [ StackPanel.children ([ header ] @ (props.Books |> List.map row)) ]
