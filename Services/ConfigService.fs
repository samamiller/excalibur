namespace Excalibur.Services

open System
open System.IO
open System.Text.Json

type AppConfig =
    { Theme: string option
      Columns: string list option }

module ConfigService =
    let private appDir () =
        let baseDir =
            match Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) with
            | null
            | "" -> Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            | v -> v

        Path.Combine(baseDir, "Excalibur")

    let private configPath () =
        Path.Combine(appDir (), "excalibur.json")

    let load () : AppConfig =
        try
            let path = configPath ()

            if File.Exists path then
                let json = File.ReadAllText path
                let cfg = JsonSerializer.Deserialize<AppConfig>(json)

                match box cfg with
                | null -> { Theme = None; Columns = None }
                | _ -> cfg
            else
                { Theme = None; Columns = None }
        with
        | _ -> { Theme = None; Columns = None }

    let save (cfg: AppConfig) =
        try
            let dir = appDir ()

            if not (Directory.Exists dir) then
                Directory.CreateDirectory dir |> ignore

            let json =
                JsonSerializer.Serialize(cfg, JsonSerializerOptions(WriteIndented = true))

            File.WriteAllText(configPath (), json)
        with
        | _ -> ()

    let setTheme (theme: string option) =
        let current = load ()
        let next = { current with Theme = theme }
        save next

    let setColumns (columns: string list) =
        let current = load ()
        let next = { current with Columns = Some columns }
        save next

    let getColumns () : string list option = (load ()).Columns
