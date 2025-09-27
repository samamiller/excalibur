module Excalibur.Views.TopBar

open Avalonia
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.Layout

type Props =
    { Query: string
      OnQueryChanged: string -> unit
      OnClear: unit -> unit
      OnAddBooks: unit -> unit }

let view props : Types.IView =
    let searchControls: Types.IView list =
        [ TextBox.create [ TextBox.watermark "Search title/author"
                           TextBox.text props.Query
                           TextBox.onTextChanged (fun txt -> props.OnQueryChanged txt)
                           TextBox.width 260.0 ]
          Button.create [ Button.content "Clear"
                          Button.onClick (fun _ -> props.OnClear()) ] ]

    let panelChildren: Types.IView list =
        [ StackPanel.create [ DockPanel.dock Avalonia.Controls.Dock.Top
                              StackPanel.orientation Orientation.Horizontal
                              StackPanel.spacing 8.0
                              StackPanel.margin (Thickness 8.0)
                              StackPanel.children searchControls ]
          Button.create [ DockPanel.dock Avalonia.Controls.Dock.Left
                          Button.content "Add book(s)"
                          Button.onClick (fun _ -> props.OnAddBooks()) ]
          TextBlock.create [ DockPanel.dock Avalonia.Controls.Dock.Right
                             TextBlock.text "Minimal Library (MVP)"
                             TextBlock.verticalAlignment VerticalAlignment.Center ] ]

    DockPanel.create [ DockPanel.lastChildFill true
                       DockPanel.children panelChildren ]
