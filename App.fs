namespace Excalibur

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Styling
open Avalonia.Themes.Fluent
open Avalonia.Markup.Xaml.Styling
open System
open Excalibur.Views

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        // Ensure TreeDataGrid styles are available
        this.Styles.Add(
            StyleInclude(baseUri = null, Source = Uri("avares://Avalonia.Controls.TreeDataGrid/Themes/Fluent.axaml"))
        )

        this.RequestedThemeVariant <- ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =
    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime(args)
