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

        Directory.CreateDirectory(if String.IsNullOrEmpty dir then "." else dir)
        |> ignore

        use conn = new SqliteConnection(connectionString)
        conn.Open()
        // bootstrap minimal table to allow ALTERs to succeed in migrations
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, author TEXT NULL, path TEXT NOT NULL);"

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
        ensureColumn "books" "sortable_title" "ALTER TABLE books ADD COLUMN sortable_title TEXT NULL"
        ensureColumn "books" "pubdate" "ALTER TABLE books ADD COLUMN pubdate TEXT NULL"
        ensureColumn "books" "series_index" "ALTER TABLE books ADD COLUMN series_index REAL NULL"
        ensureColumn "books" "author_sort" "ALTER TABLE books ADD COLUMN author_sort TEXT NULL"
        ensureColumn "books" "isbn" "ALTER TABLE books ADD COLUMN isbn TEXT NULL"
        ensureColumn "books" "series_id" "ALTER TABLE books ADD COLUMN series_id INTEGER NULL"

        // apply SQL migrations idempotently (tables, indexes)
        let migrationsPath =
            Path.Combine(Environment.CurrentDirectory, "Data", "Migrations.sql")

        if File.Exists migrationsPath then
            let sql = File.ReadAllText migrationsPath
            use m = conn.CreateCommand()
            m.CommandText <- sql
            m.ExecuteNonQuery() |> ignore

    let private readBooks (r: SqliteDataReader) =
        seq {
            while r.Read() do
                let id = r.GetInt32(0)
                let title = r.GetString(1)

                let author = if r.IsDBNull(2) then None else Some(r.GetString(2))

                let path = r.GetString(3)

                let tags = if r.IsDBNull(4) then None else Some(r.GetString(4))

                let comments = if r.IsDBNull(5) then None else Some(r.GetString(5))

                let checksum = if r.IsDBNull(6) then None else Some(r.GetString(6))

                let addedAt = if r.IsDBNull(7) then None else Some(r.GetString(7))

                let missing =
                    try
                        if r.IsDBNull(8) then false else (r.GetInt32(8) <> 0)
                    with _ ->
                        false

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

    type SortKey =
        | Title
        | Author
        | SortableTitle
        | AuthorSort
        | AddedAt
        | Missing

    type SortSpec = { Key: SortKey; Asc: bool }

    type Filters =
        { TitleContains: string option
          AuthorContains: string option
          TagContains: string option
          MissingOnly: bool option
          AuthorEquals: string option
          TagEquals: string option
          SeriesEquals: string option }

    type Page = { Offset: int; Limit: int }

    let private orderBySql (sorts: SortSpec list) =
        if List.isEmpty sorts then
            "order by title"
        else
            sorts
            |> List.map (fun s ->
                let col =
                    match s.Key with
                    | Title -> "title"
                    | Author -> "author"
                    | SortableTitle -> "sortable_title"
                    | AuthorSort -> "author_sort"
                    | AddedAt -> "added_at"
                    | Missing -> "missing"

                let dir = if s.Asc then "asc" else "desc"
                $"{col} {dir}")
            |> String.concat ","
            |> fun ob -> $"order by {ob}"

    let private whereSql (filters: Filters) =
        let clauses =
            [ match filters.TitleContains with
              | Some v when v <> "" -> yield "b.title like $ftitle"
              | _ -> ()
              match filters.AuthorContains with
              | Some v when v <> "" -> yield "b.author like $fauthor"
              | _ -> ()
              match filters.TagContains with
              | Some v when v <> "" -> yield "b.tags like $ftag"
              | _ -> ()
              match filters.AuthorEquals with
              | Some _ -> yield "a.name = $aeq"
              | None -> ()
              match filters.TagEquals with
              | Some _ -> yield "t.name = $teq"
              | None -> ()
              match filters.SeriesEquals with
              | Some _ -> yield "s.name = $seq"
              | None -> ()
              match filters.MissingOnly with
              | Some true -> yield "b.missing = 1"
              | _ -> () ]

        if List.isEmpty clauses then
            ""
        else
            "where " + String.concat " and " clauses

    let private bindFilterParams (cmd: SqliteCommand) (filters: Filters) =
        match filters.TitleContains with
        | Some v when v <> "" -> cmd.Parameters.AddWithValue("$ftitle", "%" + v + "%") |> ignore
        | _ -> ()

        match filters.AuthorContains with
        | Some v when v <> "" -> cmd.Parameters.AddWithValue("$fauthor", "%" + v + "%") |> ignore
        | _ -> ()

        match filters.TagContains with
        | Some v when v <> "" -> cmd.Parameters.AddWithValue("$ftag", "%" + v + "%") |> ignore
        | _ -> ()

        match filters.AuthorEquals with
        | Some v -> cmd.Parameters.AddWithValue("$aeq", v) |> ignore
        | None -> ()

        match filters.TagEquals with
        | Some v -> cmd.Parameters.AddWithValue("$teq", v) |> ignore
        | None -> ()

        match filters.SeriesEquals with
        | Some v -> cmd.Parameters.AddWithValue("$seq", v) |> ignore
        | None -> ()

        ()

    let getBooks () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select id,title,author,path,tags,comments,checksum,added_at,missing from books order by title"

        use r = cmd.ExecuteReader()
        readBooks r

    let queryBooks (filters: Filters) (sorts: SortSpec list) (page: Page option) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        let whereClause = whereSql filters
        let orderClause = orderBySql sorts

        let limitClause =
            match page with
            | Some p -> $" limit {p.Limit} offset {p.Offset}"
            | None -> ""

        let joins =
            [ match filters.AuthorEquals with
              | Some _ ->
                  yield "left join books_authors_link bal on bal.book_id=b.id left join authors a on a.id=bal.author_id"
              | None -> ()
              match filters.TagEquals with
              | Some _ -> yield "left join books_tags_link btl on btl.book_id=b.id left join tags t on t.id=btl.tag_id"
              | None -> ()
              match filters.SeriesEquals with
              | Some _ -> yield ""
              | None -> () ]
            |> String.concat " "

        cmd.CommandText <-
            $"select b.id,b.title,b.author,b.path,b.tags,b.comments,b.checksum,b.added_at,b.missing from books b {joins} {whereClause} {orderClause}{limitClause}"

        bindFilterParams cmd filters
        use r = cmd.ExecuteReader()
        readBooks r

    let countBooks (filters: Filters) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        let whereClause = whereSql filters

        let joins =
            [ match filters.AuthorEquals with
              | Some _ ->
                  yield "left join books_authors_link bal on bal.book_id=b.id left join authors a on a.id=bal.author_id"
              | None -> ()
              match filters.TagEquals with
              | Some _ -> yield "left join books_tags_link btl on btl.book_id=b.id left join tags t on t.id=btl.tag_id"
              | None -> ()
              match filters.SeriesEquals with
              | Some _ -> yield ""
              | None -> () ]
            |> String.concat " "

        cmd.CommandText <- $"select count(distinct b.id) from books b {joins} {whereClause}"
        bindFilterParams cmd filters

        match cmd.ExecuteScalar() with
        | :? int64 as c -> int c
        | :? int as c -> c
        | _ -> 0

    let addBook (title: string) (author: string option) (path: string) : bool =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        // Ignore if book with same path already exists
        use existsCmd = conn.CreateCommand()
        existsCmd.CommandText <- "select 1 from books where path = $p limit 1"

        existsCmd.Parameters.AddWithValue("$p", path) |> ignore

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

            cmd.Parameters.AddWithValue("$d", DateTime.UtcNow.ToString("o")) |> ignore

            let checksum =
                try
                    use sha = System.Security.Cryptography.SHA256.Create()
                    use fs = File.OpenRead(path)

                    sha.ComputeHash(fs)
                    |> BitConverter.ToString
                    |> fun hash -> hash.Replace("-", "").ToLowerInvariant()
                with _ ->
                    null

            let cParam = cmd.CreateParameter()
            cParam.ParameterName <- "$c"

            cParam.Value <- if isNull checksum then box DBNull.Value else box checksum

            cmd.Parameters.Add(cParam) |> ignore

            let rows = cmd.ExecuteNonQuery()
            rows > 0

    let setMissing (id: int) (missing: bool) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "update books set missing = $m where id = $id"

        cmd.Parameters.AddWithValue("$m", (if missing then 1 else 0)) |> ignore

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
