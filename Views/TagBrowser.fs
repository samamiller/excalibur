module Excalibur.Views.TagBrowser

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Excalibur.Services
open System

type FacetItem = { Name: string; Count: int }

type Props =
    { Authors: FacetItem list
      Series: FacetItem list
      Tags: FacetItem list
      AuthorSearch: string
      OnAuthorSearchChanged: string -> unit
      SeriesSearch: string
      OnSeriesSearchChanged: string -> unit
      TagSearch: string
      OnTagSearchChanged: string -> unit
      OnFilterByTag: string option -> unit
      OnFilterByAuthor: string option -> unit
      OnFilterBySeries: string option -> unit }

let private facetButtons items onClick =
    StackPanel.create
        [ StackPanel.spacing 4.0
          StackPanel.children (
              items
              |> List.map (fun item ->
                  Button.create
                      [ Button.content (sprintf "%s (%d)" item.Name item.Count)
                        Button.onClick (fun _ -> onClick (Some item.Name)) ])
          ) ]

let view (props: Props) : Types.IView =
    StackPanel.create
        [ StackPanel.classes [ "tag-browser" ]
          StackPanel.spacing 8.0
          StackPanel.children
              [ TextBlock.create [ TextBlock.text "Authors"; TextBlock.margin 6.0 ]
                TextBox.create
                    [ TextBox.watermark "Search authors"
                      TextBox.text props.AuthorSearch
                      TextBox.onTextChanged props.OnAuthorSearchChanged ]
                facetButtons props.Authors props.OnFilterByAuthor
                Button.create
                    [ Button.content "Clear Author"
                      Button.onClick (fun _ -> props.OnFilterByAuthor None) ]
                TextBlock.create [ TextBlock.text "Series"; TextBlock.margin 6.0 ]
                TextBox.create
                    [ TextBox.watermark "Search series"
                      TextBox.text props.SeriesSearch
                      TextBox.onTextChanged props.OnSeriesSearchChanged ]
                facetButtons props.Series props.OnFilterBySeries
                Button.create
                    [ Button.content "Clear Series"
                      Button.onClick (fun _ -> props.OnFilterBySeries None) ]
                TextBlock.create [ TextBlock.text "Tags"; TextBlock.margin 6.0 ]
                TextBox.create
                    [ TextBox.watermark "Search tags"
                      TextBox.text props.TagSearch
                      TextBox.onTextChanged props.OnTagSearchChanged ]
                facetButtons props.Tags props.OnFilterByTag
                Button.create
                    [ Button.content "Clear Tag"
                      Button.onClick (fun _ -> props.OnFilterByTag None) ] ] ]
