namespace Excalibur.Data

open System
open System.IO
open Microsoft.Data.Sqlite
open Excalibur.Domain

module LibraryRepository =
    let private dbPath = Path.Combine(Environment.CurrentDirectory, "excalibur.db")
    let private connectionString = $"Data Source={dbPath}"

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

        ensureColumn "books" "added_at" "ALTER TABLE books ADD COLUMN added_at TEXT NULL"
        ensureColumn "books" "checksum" "ALTER TABLE books ADD COLUMN checksum TEXT NULL"
        ensureColumn "books" "comments" "ALTER TABLE books ADD COLUMN comments TEXT NULL"
        ensureColumn "books" "tags" "ALTER TABLE books ADD COLUMN tags TEXT NULL"
        ensureColumn "books" "missing" "ALTER TABLE books ADD COLUMN missing INTEGER NOT NULL DEFAULT 0"

        use ci = conn.CreateCommand()

        ci.CommandText <-
            """
        CREATE INDEX IF NOT EXISTS idx_books_title ON books(title);
        CREATE INDEX IF NOT EXISTS idx_books_author ON books(author);
        CREATE INDEX IF NOT EXISTS idx_books_tags ON books(tags);
        CREATE UNIQUE INDEX IF NOT EXISTS ux_books_path ON books(path);
        """

        ci.ExecuteNonQuery() |> ignore

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

        let checksum =
            try
                use sha = System.Security.Cryptography.SHA256.Create()
                use fs = File.OpenRead(path)

                sha.ComputeHash(fs)
                |> BitConverter.ToString
                |> fun hash -> hash.Replace("-", "").ToLowerInvariant()
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
