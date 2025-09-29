namespace Excalibur.Services

open System
open System.IO

module Logger =
    let private appDir () =
        let baseDir =
            match Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) with
            | null
            | "" -> Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            | v -> v

        Path.Combine(baseDir, "Excalibur")

    let private logPath () =
        Path.Combine(appDir (), "excalibur.log")

    let private write (level: string) (msg: string) =
        let line =
            sprintf "%s [%s] %s" (DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")) level msg

        try
            Console.WriteLine(line)
            let dir = appDir ()

            if not (Directory.Exists dir) then
                Directory.CreateDirectory dir |> ignore

            File.AppendAllText(logPath (), line + Environment.NewLine)
        with
        | _ -> ()

    let info (msg: string) = write "INFO" msg
    let warn (msg: string) = write "WARN" msg
    let error (msg: string) = write "ERROR" msg

    let infof fmt = Printf.kprintf info fmt
    let warnf fmt = Printf.kprintf warn fmt
    let errorf fmt = Printf.kprintf error fmt
