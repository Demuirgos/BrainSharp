open System
open Interpreter

[<EntryPoint>]
let main argv =
    let arg = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let result = interpret arg
    printfn "%A %d" result (arg.Length)
    0 