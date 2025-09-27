open System
open System.IO
open System.Data
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Microsoft.Data.Sqlite

module Data =
    let dbPath = Path.Combine(Environment.CurrentDirectory, "excalibur.db")
    let connectionString = $"Data Source={dbPath}"

    let ensureDb () =
        let dir = Path.GetDirectoryName dbPath

        Directory.CreateDirectory(
            if String.IsNullOrEmpty dir then
                "."
            else
                dir
        )
        |> ignore

        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            """
        CREATE TABLE IF NOT EXISTS books (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            title TEXT NOT NULL,
            author TEXT NULL,
            path TEXT NOT NULL
        );
        """

        cmd.ExecuteNonQuery() |> ignore

    type Book =
        { id: int
          title: string
          author: string option
          path: string }

    let private readBooks (r: SqliteDataReader) =
        seq {
            while r.Read() do
                let id = r.GetInt32(0)
                let title = r.GetString(1)

                let author =
                    if r.IsDBNull(2) then
                        None
                    else
                        Some(r.GetString(2))

                let path = r.GetString(3)

                yield
                    { id = id
                      title = title
                      author = author
                      path = path }
        }
        |> List.ofSeq

    let getBooks () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "select id,title,author,path from books order by title"
        use r = cmd.ExecuteReader()
        readBooks r

    let addBook (title: string) (author: string option) (path: string) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "insert into books(title,author,path) values ($t,$a,$p)"
        cmd.Parameters.AddWithValue("$t", title) |> ignore

        cmd.Parameters.AddWithValue("$a", (author |> Option.defaultValue null))
        |> ignore

        cmd.Parameters.AddWithValue("$p", path) |> ignore
        cmd.ExecuteNonQuery() |> ignore



type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Excalibur"
        Data.ensureDb ()

        let mutable booksState: Data.Book list = Data.getBooks ()
        let setBooks (v: Data.Book list) = booksState <- v

        let addFromPath (file: string) =
            let title = Path.GetFileNameWithoutExtension file
            Data.addBook title None file
            setBooks (Data.getBooks ())

        let topBar: Types.IView =
            DockPanel.create [ DockPanel.lastChildFill true
                               DockPanel.children [ Button.create [ DockPanel.dock Dock.Left
                                                                    Button.content "Add book(s)"
                                                                    Button.onClick (fun _ ->
                                                                        let ofd = OpenFileDialog(AllowMultiple = true)

                                                                        ofd.Filters.Add(
                                                                            FileDialogFilter(
                                                                                Name = "E-books",
                                                                                Extensions =
                                                                                    Collections.Generic.List<string>(
                                                                                        [ "epub"
                                                                                          "pdf"
                                                                                          "mobi"
                                                                                          "azw3" ]
                                                                                    )
                                                                            )
                                                                        )

                                                                        let files =
                                                                            ofd.ShowAsync(this)
                                                                            |> Async.AwaitTask
                                                                            |> Async.RunSynchronously

                                                                        let files = if isNull files then [||] else files
                                                                        files |> Array.iter addFromPath) ]
                                                    TextBlock.create [ DockPanel.dock Dock.Right
                                                                       TextBlock.text "Minimal Library (MVP)"
                                                                       TextBlock.verticalAlignment
                                                                           VerticalAlignment.Center ] ] ]

        let bookListView: Types.IView =
            ListBox.create [ ListBox.dataItems booksState
                             ListBox.itemTemplate (
                                 DataTemplateView<Data.Book>.create
                                     (fun b ->
                                         StackPanel.create [ StackPanel.orientation Orientation.Vertical
                                                             StackPanel.children [ TextBlock.create [ TextBlock.text
                                                                                                          b.title ]
                                                                                   TextBlock.create [ TextBlock.text (
                                                                                                          defaultArg
                                                                                                              b.author
                                                                                                              "Unknown author"
                                                                                                      )
                                                                                                      TextBlock.classes [ "subtle" ] ]
                                                                                   TextBlock.create [ TextBlock.text
                                                                                                          b.path
                                                                                                      TextBlock.fontSize
                                                                                                          10.0 ] ] ])
                             ) ]

        base.Content <-
            DockPanel.create [ DockPanel.children [ topBar
                                                    ScrollViewer.create [ DockPanel.dock Dock.Bottom
                                                                          ScrollViewer.content (bookListView) ] ] ]

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()


[<EntryPoint>]
let main (args: string []) =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(args)
