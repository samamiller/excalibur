namespace Excalibur.Services

open System
open System.IO
open Microsoft.Data.Sqlite

module AuthorsRepository =
    let private dbPath = Path.Combine(Environment.CurrentDirectory, "excalibur.db")
    let private connectionString = $"Data Source={dbPath}"

    let listWithCounts () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select a.id,a.name,COALESCE(a.sort,''),COUNT(bal.book_id) from authors a left join books_authors_link bal on bal.author_id=a.id group by a.id,a.name,a.sort order by a.sort,a.name"

        use r = cmd.ExecuteReader()

        [ while r.Read() do
              yield (r.GetInt32(0), r.GetString(1), r.GetString(2), r.GetInt32(3)) ]

module TagsRepository =
    let private dbPath = Path.Combine(Environment.CurrentDirectory, "excalibur.db")
    let private connectionString = $"Data Source={dbPath}"

    let listWithCounts () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select t.id,t.name,COUNT(btl.book_id) from tags t left join books_tags_link btl on btl.tag_id=t.id group by t.id,t.name order by t.name"

        use r = cmd.ExecuteReader()

        [ while r.Read() do
              yield (r.GetInt32(0), r.GetString(1), r.GetInt32(2)) ]

    let listWithCountsLike (query: string) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select t.id,t.name,COUNT(btl.book_id) from tags t left join books_tags_link btl on btl.tag_id=t.id where t.name like $q group by t.id,t.name order by t.name"

        cmd.Parameters.AddWithValue("$q", "%" + query + "%") |> ignore
        use r = cmd.ExecuteReader()

        [ while r.Read() do
              yield (r.GetInt32(0), r.GetString(1), r.GetInt32(2)) ]

module SeriesRepository =
    let private dbPath = Path.Combine(Environment.CurrentDirectory, "excalibur.db")
    let private connectionString = $"Data Source={dbPath}"

    let listAll () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()
        cmd.CommandText <- "select id,name from series order by name"
        use r = cmd.ExecuteReader()

        [ while r.Read() do
              yield (r.GetInt32(0), r.GetString(1)) ]

    let listWithCounts () =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select s.id,s.name,COUNT(b.id) from series s left join books b on b.series_id = s.id group by s.id,s.name order by s.name"

        use r = cmd.ExecuteReader()

        [ while r.Read() do
              yield (r.GetInt32(0), r.GetString(1), r.GetInt32(2)) ]

    let listWithCountsLike (query: string) =
        use conn = new SqliteConnection(connectionString)
        conn.Open()
        use cmd = conn.CreateCommand()

        cmd.CommandText <-
            "select s.id,s.name,COUNT(b.id) from series s left join books b on b.series_id = s.id where s.name like $q group by s.id,s.name order by s.name"

        cmd.Parameters.AddWithValue("$q", "%" + query + "%") |> ignore
        use r = cmd.ExecuteReader()

        [ while r.Read() do
              yield (r.GetInt32(0), r.GetString(1), r.GetInt32(2)) ]
