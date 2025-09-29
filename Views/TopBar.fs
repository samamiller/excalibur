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
      OnAddBooks: unit -> unit
      OnImportFolder: unit -> unit
      Theme: string
      OnThemeToggle: unit -> unit
      Status: string option }

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
          StackPanel.create [ DockPanel.dock Avalonia.Controls.Dock.Left
                              StackPanel.orientation Orientation.Horizontal
                              StackPanel.spacing 6.0
                              StackPanel.children [ Button.create [ Button.content "Add book(s)"
                                                                    Button.onClick (fun _ -> props.OnAddBooks()) ]
                                                    Button.create [ Button.content "Import from Folder"
                                                                    Button.onClick (fun _ -> props.OnImportFolder()) ] ] ]
          let rightChildren: Types.IView list =
              [ match props.Status with
                | Some s when s <> "" ->
                    yield
                        (TextBlock.create [ TextBlock.text s
                                            TextBlock.verticalAlignment VerticalAlignment.Center ]
                        :> Types.IView)
                | _ -> ()
                yield
                    (TextBlock.create [ TextBlock.text "Theme:"
                                        TextBlock.verticalAlignment VerticalAlignment.Center ]
                    :> Types.IView)
                yield
                    (Button.create [ Button.content (sprintf "%s" props.Theme)
                                     Button.onClick (fun _ -> props.OnThemeToggle()) ]
                    :> Types.IView) ]

          StackPanel.create [ DockPanel.dock Avalonia.Controls.Dock.Right
                              StackPanel.orientation Orientation.Horizontal
                              StackPanel.spacing 8.0
                              StackPanel.children rightChildren ] ]

    DockPanel.create [ DockPanel.lastChildFill true
                       DockPanel.children panelChildren ]
