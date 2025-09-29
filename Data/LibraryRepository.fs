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

                let addedAt =
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
                    { Id = id
                      Title = title
                      Author = author
                      Path = path
                      Tags = tags
                      Comments = comments
                      Checksum = checksum
                      AddedAt = addedAt
                      Missing = missing }
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

    let addBook (title: string) (author: string option) (path: string) : bool =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        // Ignore if book with same path already exists
        use existsCmd = conn.CreateCommand()
        existsCmd.CommandText <- "select 1 from books where path = $p limit 1"

        existsCmd.Parameters.AddWithValue("$p", path)
        |> ignore

        let exists =
            match existsCmd.ExecuteScalar() with
            | null -> false
            | :? DBNull -> false
            | _ -> true

        if exists then
            false
        else
            use cmd = conn.CreateCommand()
            cmd.CommandText <- "insert into books(title,author,path,added_at,checksum) values ($t,$a,$p,$d,$c)"
            cmd.Parameters.AddWithValue("$t", title) |> ignore
            // Sqlite AddWithValue does not accept null directly; parameter must exist with DBNull.Value
            let aParam = cmd.CreateParameter()
            aParam.ParameterName <- "$a"

            aParam.Value <-
                match author with
                | Some v -> box v
                | None -> box DBNull.Value

            cmd.Parameters.Add(aParam) |> ignore

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

            let cParam = cmd.CreateParameter()
            cParam.ParameterName <- "$c"

            cParam.Value <-
                if isNull checksum then
                    box DBNull.Value
                else
                    box checksum

            cmd.Parameters.Add(cParam) |> ignore

            let rows = cmd.ExecuteNonQuery()
            rows > 0

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

            let aParam = cmd.CreateParameter()
            aParam.ParameterName <- "$a"

            aParam.Value <-
                match author with
                | Some v -> box v
                | None -> box DBNull.Value

            cmd.Parameters.Add(aParam) |> ignore

            let gParam = cmd.CreateParameter()
            gParam.ParameterName <- "$g"

            gParam.Value <-
                match tags with
                | Some v -> box v
                | None -> box DBNull.Value

            cmd.Parameters.Add(gParam) |> ignore

            cmd.Parameters.AddWithValue("$id", bid) |> ignore
            cmd.ExecuteNonQuery() |> ignore

        tx.Commit()

    let updateMetadata
        (id: int)
        (title: string option)
        (author: string option)
        (tags: string option)
        (comments: string option)
        =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "update books set title = COALESCE($t, title), author = COALESCE($a, author), tags = COALESCE($g, tags), comments = COALESCE($c, comments) where id = $id"

        let tParam2 = cmd.CreateParameter()
        tParam2.ParameterName <- "$t"

        tParam2.Value <-
            (match title with
             | Some v -> box v
             | None -> box DBNull.Value)

        cmd.Parameters.Add(tParam2) |> ignore
        let aParam2 = cmd.CreateParameter()
        aParam2.ParameterName <- "$a"

        aParam2.Value <-
            (match author with
             | Some v -> box v
             | None -> box DBNull.Value)

        cmd.Parameters.Add(aParam2) |> ignore
        let gParam2 = cmd.CreateParameter()
        gParam2.ParameterName <- "$g"

        gParam2.Value <-
            (match tags with
             | Some v -> box v
             | None -> box DBNull.Value)

        cmd.Parameters.Add(gParam2) |> ignore
        let cParam2 = cmd.CreateParameter()
        cParam2.ParameterName <- "$c"

        cParam2.Value <-
            (match comments with
             | Some v -> box v
             | None -> box DBNull.Value)

        cmd.Parameters.Add(cParam2) |> ignore
        cmd.Parameters.AddWithValue("$id", id) |> ignore
        cmd.ExecuteNonQuery() |> ignore

    let deleteBooks (ids: int list) =
        if ids.Length > 0 then
            use conn = new SqliteConnection(connectionString)
            conn.Open()
            use tx = conn.BeginTransaction()

            for bid in ids do
                use cmd = conn.CreateCommand()
                cmd.Transaction <- tx
                cmd.CommandText <- "delete from books where id = $id"
                cmd.Parameters.AddWithValue("$id", bid) |> ignore
                cmd.ExecuteNonQuery() |> ignore

            tx.Commit()
