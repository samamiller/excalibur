module Excalibur.Views.BookList

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Excalibur.Domain

type Props =
    { Books: Book list
      OnSelectionChanged: int list -> unit }

let private renderBook (book: Book) : Types.IView =
    StackPanel.create [ StackPanel.orientation Orientation.Vertical
                        StackPanel.children [ TextBlock.create [ TextBlock.text book.title ]
                                              TextBlock.create [ TextBlock.text (
                                                                     defaultArg book.author "Unknown author"
                                                                 )
                                                                 TextBlock.classes [ "subtle" ] ]
                                              TextBlock.create [ TextBlock.text book.path
                                                                 TextBlock.fontSize 10.0 ]
                                              (if book.missing then
                                                   TextBlock.create [ TextBlock.text "Missing file"
                                                                      TextBlock.classes [ "subtle" ]
                                                                      TextBlock.foreground Brushes.Orange ]
                                                   :> Types.IView
                                               else
                                                   TextBlock.create [ TextBlock.text "" ] :> Types.IView) ] ]

let view props : Types.IView =
    ListBox.create [ ListBox.selectionMode SelectionMode.Multiple
                     ListBox.dataItems props.Books
                     ListBox.onSelectionChanged (fun args ->
                         match args.Source with
                         | :? ListBox as lb ->
                             lb.SelectedItems
                             |> Seq.cast<Book>
                             |> Seq.map (fun b -> b.id)
                             |> Seq.toList
                             |> props.OnSelectionChanged
                         | _ -> ())
                     ListBox.itemTemplate (DataTemplateView<Book>.create renderBook) ]
