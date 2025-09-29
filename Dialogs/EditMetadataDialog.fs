namespace Excalibur.Dialogs

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Layout

module EditMetadataDialog =
    type Result =
        { Author: string option
          Tags: string option }

    let private toOption (text: string) =
        if String.IsNullOrWhiteSpace text then None else Some text

    let show (owner: Window) =
        async {
            let dialog = Window(Width = 400.0, Height = 220.0, Title = "Batch Edit")

            let tbAuthor = TextBox(Watermark = "Author")

            let tbTags = TextBox(Watermark = "Tags (comma separated)")

            let btnSave = Button(Content = "Save")

            btnSave.Click.Add(fun _ -> dialog.Close(box true))

            let panel = StackPanel(Spacing = 8.0, Margin = Thickness 12.0)

            panel.Children.Add(TextBlock(Text = "Set Author and/or Tags for selection"))

            panel.Children.Add(tbAuthor)
            panel.Children.Add(tbTags)
            panel.Children.Add(btnSave)
            dialog.Content <- panel

            let! ok = dialog.ShowDialog<bool>(owner) |> Async.AwaitTask

            if ok then
                return
                    Some
                        { Author = toOption tbAuthor.Text
                          Tags = toOption tbTags.Text }
            else
                return None
        }
