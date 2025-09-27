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

        // create base table
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

        // migrations: add columns if missing
        let ensureColumn (table: string) (col: string) (defSql: string) =
            use c2 = conn.CreateCommand()
            c2.CommandText <- $"PRAGMA table_info({table})"
            use r = c2.ExecuteReader()
            let mutable exists = false

            while r.Read() do
                let name = r.GetString(1)

                if String.Equals(name, col, StringComparison.OrdinalIgnoreCase) then
                    exists <- true

            if not exists then
                use c3 = conn.CreateCommand()
                c3.CommandText <- defSql
                c3.ExecuteNonQuery() |> ignore

        // added_at timestamp, checksum, comments, tags (simple CSV), and missing flag
        ensureColumn "books" "added_at" "ALTER TABLE books ADD COLUMN added_at TEXT NULL"
        ensureColumn "books" "checksum" "ALTER TABLE books ADD COLUMN checksum TEXT NULL"
        ensureColumn "books" "comments" "ALTER TABLE books ADD COLUMN comments TEXT NULL"
        ensureColumn "books" "tags" "ALTER TABLE books ADD COLUMN tags TEXT NULL"
        ensureColumn "books" "missing" "ALTER TABLE books ADD COLUMN missing INTEGER NOT NULL DEFAULT 0"

        // indices for search performance
        use ci = conn.CreateCommand()

        ci.CommandText <-
            """
        CREATE INDEX IF NOT EXISTS idx_books_title ON books(title);
        CREATE INDEX IF NOT EXISTS idx_books_author ON books(author);
        CREATE INDEX IF NOT EXISTS idx_books_tags ON books(tags);
        CREATE UNIQUE INDEX IF NOT EXISTS ux_books_path ON books(path);
        """

        ci.ExecuteNonQuery() |> ignore

    type Book =
        { id: int
          title: string
          author: string option
          path: string
          tags: string option
          comments: string option
          checksum: string option
          added_at: string option
          missing: bool }

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

                let tags =
                    if r.IsDBNull(4) then
                        None
                    else
                        Some(r.GetString(4))

                let comments =
                    if r.IsDBNull(5) then
                        None
                    else
                        Some(r.GetString(5))

                let checksum =
                    if r.IsDBNull(6) then
                        None
                    else
                        Some(r.GetString(6))

                let added_at =
                    if r.IsDBNull(7) then
                        None
                    else
                        Some(r.GetString(7))

                let missing =
                    try
                        if r.IsDBNull(8) then
                            false
                        else
                            (r.GetInt32(8) <> 0)
                    with
                    | _ -> false

                yield
                    { id = id
                      title = title
                      author = author
                      path = path
                      tags = tags
                      comments = comments
                      checksum = checksum
                      added_at = added_at
                      missing = missing }
        }
        |> List.ofSeq

    let getBooks () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select id,title,author,path,tags,comments,checksum,added_at,missing from books order by title"

        use r = cmd.ExecuteReader()
        readBooks r

    let addBook (title: string) (author: string option) (path: string) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "insert into books(title,author,path,added_at,checksum) values ($t,$a,$p,$d,$c)"
        cmd.Parameters.AddWithValue("$t", title) |> ignore

        cmd.Parameters.AddWithValue("$a", (author |> Option.defaultValue null))
        |> ignore

        cmd.Parameters.AddWithValue("$p", path) |> ignore

        cmd.Parameters.AddWithValue("$d", DateTime.UtcNow.ToString("o"))
        |> ignore
        // compute simple SHA256 checksum (best-effort)
        let checksum =
            try
                use sha = System.Security.Cryptography.SHA256.Create()
                use fs = File.OpenRead(path)
                let hash = sha.ComputeHash(fs)

                System
                    .BitConverter
                    .ToString(hash)
                    .Replace("-", "")
                    .ToLowerInvariant()
            with
            | _ -> null

        cmd.Parameters.AddWithValue("$c", checksum)
        |> ignore

        cmd.ExecuteNonQuery() |> ignore

    let setMissing (id: int) (missing: bool) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "update books set missing = $m where id = $id"

        cmd.Parameters.AddWithValue("$m", (if missing then 1 else 0))
        |> ignore

        cmd.Parameters.AddWithValue("$id", id) |> ignore
        cmd.ExecuteNonQuery() |> ignore

    let updateAuthorTags (ids: int list) (author: string option) (tags: string option) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use tx = conn.BeginTransaction()

        for bid in ids do
            use cmd = conn.CreateCommand()
            cmd.Transaction <- tx

            cmd.CommandText <-
                "update books set author = COALESCE($a, author), tags = COALESCE($g, tags) where id = $id"

            cmd.Parameters.AddWithValue("$a", (author |> Option.defaultValue null))
            |> ignore

            cmd.Parameters.AddWithValue("$g", (tags |> Option.defaultValue null))
            |> ignore

            cmd.Parameters.AddWithValue("$id", bid) |> ignore
            cmd.ExecuteNonQuery() |> ignore

        tx.Commit()



type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Excalibur"
        Data.ensureDb ()

        let loadBooks () = Data.getBooks ()

        this.Content <-
            Component (fun ctx ->
                let booksState = ctx.useState (loadBooks ())
                let query = ctx.useState ""
                let selectedIds = ctx.useState ([]: int list)

                let filteredBooks =
                    let books = booksState.Current

                    if String.IsNullOrWhiteSpace query.Current then
                        books
                    else
                        let q = query.Current.Trim().ToLowerInvariant()

                        books
                        |> List.filter (fun b ->
                            b.title.ToLowerInvariant().Contains q
                            || (b.author
                                |> Option.defaultValue ""
                                |> fun a -> a.ToLowerInvariant().Contains q))

                let refreshBooks () =
                    booksState.Set(loadBooks ())
                    selectedIds.Set []

                let addFromPath (file: string) =
                    let title = Path.GetFileNameWithoutExtension file
                    Data.addBook title None file
                    refreshBooks ()

                let refreshAndMarkMissing () =
                    let bs = Data.getBooks ()

                    for b in bs do
                        let exists = File.Exists b.path

                        if exists = b.missing then
                            Data.setMissing b.id (not exists)

                    refreshBooks ()

                let topBar: Types.IView =
                    DockPanel.create [ DockPanel.lastChildFill true
                                       DockPanel.children [ StackPanel.create [ DockPanel.dock Dock.Top
                                                                                StackPanel.orientation
                                                                                    Orientation.Horizontal
                                                                                StackPanel.spacing 8.0
                                                                                StackPanel.margin (Thickness 8.0)
                                                                                StackPanel.children [ TextBox.create [ TextBox.watermark
                                                                                                                           "Search title/author"
                                                                                                                       TextBox.text
                                                                                                                           query.Current
                                                                                                                       TextBox.onTextChanged
                                                                                                                           (fun txt ->
                                                                                                                               query.Set
                                                                                                                                   txt

                                                                                                                               selectedIds.Set [  ])
                                                                                                                       TextBox.width
                                                                                                                           260.0 ]
                                                                                                      Button.create [ Button.content
                                                                                                                          "Clear"
                                                                                                                      Button.onClick
                                                                                                                          (fun _ ->
                                                                                                                              query.Set
                                                                                                                                  ""

                                                                                                                              selectedIds.Set [  ]) ] ] ]
                                                            Button.create [ DockPanel.dock Dock.Left
                                                                            Button.content "Add book(s)"
                                                                            Button.onClick (fun _ ->
                                                                                let ofd =
                                                                                    OpenFileDialog(AllowMultiple = true)

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

                                                                                let files =
                                                                                    if isNull files then [||] else files

                                                                                files |> Array.iter addFromPath
                                                                                refreshAndMarkMissing ()) ]
                                                            TextBlock.create [ DockPanel.dock Dock.Right
                                                                               TextBlock.text "Minimal Library (MVP)"
                                                                               TextBlock.verticalAlignment
                                                                                   VerticalAlignment.Center ] ] ]

                let bookListView: Types.IView =
                    ListBox.create [ ListBox.selectionMode SelectionMode.Multiple
                                     ListBox.dataItems filteredBooks
                                     ListBox.onSelectionChanged (fun args ->
                                         match args.Source with
                                         | :? ListBox as lb ->
                                             lb.SelectedItems
                                             |> Seq.cast<Data.Book>
                                             |> Seq.map (fun b -> b.id)
                                             |> Seq.toList
                                             |> selectedIds.Set
                                         | _ -> ())
                                     ListBox.itemTemplate (
                                         DataTemplateView<Data.Book>.create
                                             (fun b ->
                                                 StackPanel.create [ StackPanel.orientation Orientation.Vertical
                                                                     StackPanel.children [ TextBlock.create [ TextBlock.text
                                                                                                                  b.title ]
                                                                                           TextBlock.create [ TextBlock
                                                                                                                  .text (
                                                                                                                      defaultArg
                                                                                                                          b.author
                                                                                                                          "Unknown author"
                                                                                                                  )
                                                                                                              TextBlock.classes [ "subtle" ] ]
                                                                                           TextBlock.create [ TextBlock.text
                                                                                                                  b.path
                                                                                                              TextBlock.fontSize
                                                                                                                  10.0 ]
                                                                                           (if b.missing then
                                                                                                TextBlock.create [ TextBlock.text
                                                                                                                       "Missing file"
                                                                                                                   TextBlock.classes [ "subtle" ]
                                                                                                                   TextBlock
                                                                                                                       .foreground (
                                                                                                                           Media.Brushes.Orange
                                                                                                                       ) ]
                                                                                                :> Types.IView
                                                                                            else
                                                                                                TextBlock.create [ TextBlock.text
                                                                                                                       "" ]
                                                                                                :> Types.IView) ] ])
                                     ) ]

                let editBar: Types.IView =
                    StackPanel.create [ DockPanel.dock Dock.Bottom
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.spacing 8.0
                                        StackPanel.margin (Thickness 8.0)
                                        StackPanel.children [ Button.create [ Button.content "Batch Edit (Author/Tags)"
                                                                              Button.onClick (fun _ ->
                                                                                  let w =
                                                                                      new Window(
                                                                                          Width = 400.0,
                                                                                          Height = 220.0,
                                                                                          Title = "Batch Edit"
                                                                                      )

                                                                                  let tbAuthor =
                                                                                      new TextBox(Watermark = "Author")

                                                                                  let tbTags =
                                                                                      new TextBox(
                                                                                          Watermark =
                                                                                              "Tags (comma separated)"
                                                                                      )

                                                                                  let btn = new Button(Content = "Save")

                                                                                  btn.Click.Add (fun _ ->
                                                                                      w.Close(box true))

                                                                                  let panel =
                                                                                      new StackPanel(
                                                                                          Spacing = 8.0,
                                                                                          Margin = Thickness 12.0
                                                                                      )

                                                                                  panel.Children.Add(
                                                                                      new TextBlock(
                                                                                          Text =
                                                                                              "Set Author and/or Tags for selection"
                                                                                      )
                                                                                  )

                                                                                  panel.Children.Add(tbAuthor)
                                                                                  panel.Children.Add(tbTags)
                                                                                  panel.Children.Add(btn)
                                                                                  w.Content <- panel

                                                                                  let ok =
                                                                                      w.ShowDialog<bool>(this)
                                                                                      |> Async.AwaitTask
                                                                                      |> Async.RunSynchronously

                                                                                  if ok
                                                                                     && selectedIds.Current.Length > 0 then
                                                                                      Data.updateAuthorTags
                                                                                          selectedIds.Current
                                                                                          (if
                                                                                               String.IsNullOrWhiteSpace
                                                                                                   tbAuthor.Text then
                                                                                               None
                                                                                           else
                                                                                               Some tbAuthor.Text)
                                                                                          (if
                                                                                               String.IsNullOrWhiteSpace
                                                                                                   tbTags.Text then
                                                                                               None
                                                                                           else
                                                                                               Some tbTags.Text)

                                                                                      refreshBooks ()) ] ] ]

                let dropHost: Types.IView =
                    Border.create [ Border.child (
                                        ScrollViewer.create [ DockPanel.dock Dock.Bottom
                                                              ScrollViewer.content bookListView ]
                                    )
                                    Border.onPointerPressed (fun _ -> ())
                                    (* Drag/drop wiring omitted for now; see original comments. *)
                                     ]

                DockPanel.create [ DockPanel.children [ topBar
                                                        dropHost
                                                        editBar ] ])

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
