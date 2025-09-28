module Excalibur.Views.BookDetails

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Excalibur.Domain

type Props = { Selected: Book option }

let private placeholder: Types.IView =
    TextBlock.create [ TextBlock.text "No book selected"
                       TextBlock.margin 8.0 ]

let private details (b: Book) : Types.IView =
    ScrollViewer.create [ ScrollViewer.content (
                              StackPanel.create [ StackPanel.spacing 6.0
                                                  StackPanel.margin 8.0
                                                  StackPanel.children [ TextBlock.create [ TextBlock.text b.title
                                                                                           TextBlock.fontSize 18.0 ]
                                                                        TextBlock.create [ TextBlock.text (
                                                                                               defaultArg
                                                                                                   b.author
                                                                                                   "Unknown author"
                                                                                           ) ]
                                                                        TextBlock.create [ TextBlock.text (
                                                                                               sprintf "Path: %s" b.path
                                                                                           ) ]
                                                                        (match b.tags with
                                                                         | Some t when t <> "" ->
                                                                             TextBlock.create [ TextBlock.text (
                                                                                                    sprintf "Tags: %s" t
                                                                                                ) ]
                                                                         | _ ->
                                                                             TextBlock.create [ TextBlock.text "Tags: —" ])
                                                                        (match b.comments with
                                                                         | Some c when c <> "" ->
                                                                             TextBlock.create [ TextBlock.text c
                                                                                                TextBlock.textWrapping
                                                                                                    TextWrapping.Wrap ]
                                                                         | _ -> TextBlock.create [ TextBlock.text "" ]) ] ]
                          ) ]

let view props : Types.IView =
    match props.Selected with
    | Some b ->
        Border.create [ Border.classes [ "details-pane" ]
                        Border.child (details b) ]
    | None ->
        Border.create [ Border.classes [ "details-pane" ]
                        Border.child placeholder ]
