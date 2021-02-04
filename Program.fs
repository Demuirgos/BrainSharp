open System
open Interpreter

[<EntryPoint>]
let REPL =
    let prefix = "# >"
    let read = Console.ReadLine >> sprintf "%s"
    let eval = interpret
    let print= 
        function 
        | Some(result)  ->  printfn "%s" result
                            prefix
        | _ -> printfn ""
               prefix
    let rec loop prefix = 
        prefix |> printf "%s"
        |> read |> interpret |> print |> loop
    loop prefix
