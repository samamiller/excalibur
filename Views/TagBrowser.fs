module Excalibur.Views.TagBrowser

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Excalibur.Domain

type Props =
    { Books: Book list
      OnFilterByTag: string option -> unit }

let private groupTags (books: Book list) =
    books
    |> List.collect (fun b ->
        match b.Tags with
        | Some t when t.Trim() <> "" ->
            t.Split([| ',' |])
            |> Array.toList
            |> List.map (fun s -> s.Trim())
        | _ -> [])
    |> List.groupBy id
    |> List.map (fun (tag, items) -> tag, List.length items)
    |> List.sortByDescending snd

let view props : Types.IView =
    let items =
        groupTags props.Books
        |> List.map (fun (tag, count) -> sprintf "%s (%d)" tag count)

    StackPanel.create [ StackPanel.classes [ "tag-browser" ]
                        StackPanel.children [ TextBlock.create [ TextBlock.text "Tags"
                                                                 TextBlock.margin 6.0 ]
                                              ListBox.create [ ListBox.dataItems ("All" :: items)
                                                               ListBox.onDoubleTapped (fun args ->
                                                                   match args.Source with
                                                                   | :? ListBox as lb ->
                                                                       match lb.SelectedItem with
                                                                       | :? string as s when s = "All" ->
                                                                           props.OnFilterByTag None
                                                                       | :? string as s ->
                                                                           let tag = s.Split('(').[0].TrimEnd()
                                                                           props.OnFilterByTag(Some tag)
                                                                       | _ -> ()
                                                                   | _ -> ()) ] ] ]
