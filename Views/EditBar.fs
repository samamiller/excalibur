module Excalibur.Views.EditBar

open Avalonia
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.Layout

type Props = { OnBatchEdit: unit -> unit }

let view props : Types.IView =
    let buttons: Types.IView list =
        [ Button.create [ Button.content "Batch Edit (Author/Tags)"
                          Button.onClick (fun _ -> props.OnBatchEdit()) ] ]

    StackPanel.create [ DockPanel.dock Avalonia.Controls.Dock.Bottom
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 8.0
                        StackPanel.margin (Thickness 8.0)
                        StackPanel.children buttons ]
