module Interpreter
    open System
    type IO = 
        | Bucket of char list
        | Empty
    and Program = 
        | Instructions of char list
        | Idle
    and State = 
        {
            Memory : int list
            Index  : int
            Pointer: int
            Program: Program
            Output : IO
        }
        static member Default = {
            Memory  = List.init 256 (fun _ -> 0)
            Index   = 0
            Pointer = 0
            Program = Idle
            Output  = Empty
        }
        member this.Length = match this.Program with 
                             | Instructions(is) -> List.length is
                             | _ -> -1 
    let (|State|) s = s.Memory, s.Program, s.Output
    let interpret program=
        let rec loop acc stream =
            match stream with 
            | Ok(state) ->
                if state.Index = state.Length then 
                    Ok state
                else 
                    let next = match state.Program  with 
                                | Instructions(is) -> 
                                    match is.[state.Index] with 
                                    | '<' -> 
                                        acc, Ok  { state with Pointer = state.Pointer - 1
                                                              Index   = state.Index + 1 
                                        } 
                                    | '>' ->
                                        if state.Memory.Length - 1 = state.Pointer then
                                            acc, Ok { state with Memory  = state.Memory@List.init 32 (fun _ -> 0)
                                                                 Pointer = state.Pointer + 1
                                                                 Index   = state.Index + 1 
                                            }
                                        else 
                                            acc, Ok { state with Pointer = state.Pointer + 1 
                                                                 Index   = state.Index + 1
                                            }
                                    | '+' ->
                                        acc, Ok { state with Memory = List.mapi (fun i e -> if i <> state.Pointer then e else e + 1) state.Memory
                                                             Index = state.Index + 1
                                        }
                                    | '-' ->
                                        acc , Ok { state with Memory = List.mapi (fun i e -> if i <> state.Pointer then e else e - 1) state.Memory 
                                                              Index = state.Index + 1
                                        }  
                                    | '[' ->
                                        match state.Memory.[state.Pointer] with 
                                        | 0 -> 
                                            let rec consume state acc= 
                                                match (acc, is.[state.Index], is.[state.Index + 1]) with 
                                                | ( 0 , ']', _) -> acc, Ok { state  with Index = state.Index + 1}
                                                | ( _ ,  _ , c) -> consume {
                                                                    state with Index = state.Index + 1
                                                                   } (match c with ']' -> (acc - 1) | '[' -> (acc + 1) | _ -> acc)
                                            (state, acc + 1) ||> consume
                                        | _ -> acc, Ok {state with Index = state.Index + 1}
                                    | ']' ->
                                        match state.Memory.[state.Pointer]  with 
                                        | 0 -> acc, Ok {state with Index = state.Index + 1}
                                        | _ -> 
                                            let rec consume state acc= 
                                                match (acc, is.[state.Index], is.[state.Index - 1]) with 
                                                | ( 0 , '[', _) -> acc, Ok { state  with Index = state.Index + 1}
                                                | ( _ ,  _ , c) -> consume {
                                                                    state with Index = state.Index - 1
                                                                   } (match c with ']' -> (acc + 1) | '[' -> (acc - 1) | _ -> acc)
                                            (state, acc + 1) ||> consume 
                                    | '.' -> 
                                        acc, Ok {
                                            state with Output = match state.Output with 
                                                                | Bucket(r) -> 
                                                                    Bucket (Convert.ToChar(state.Memory.[state.Pointer])::r)
                                                                | Empty -> 
                                                                    Bucket ([Convert.ToChar(state.Memory.[state.Pointer])])
                                                       Index = state.Index + 1
                                        } 
                                    | ',' -> 
                                        acc, Ok {
                                            state with  Memory = List.mapi (fun i e -> if i = state.Pointer then Console.Read() else e) state.Memory
                                                        Index = state.Index + 1
                                        } 
                                    | ' ' -> 
                                        acc, Ok {
                                            state with  Index = state.Index + 1
                                        } 
                                    | invalid -> acc, Error(sprintf "%c is not a valid BF character" invalid)
                                | Idle -> acc, Error("Program Empty")
                    next ||> loop
            | Error(_) as e -> e
        let result =    match Seq.toList program with
                        | [] -> {
                            State.Default with Program = Idle
                            }
                        | chars -> 
                            {
                                State.Default with Program = Instructions chars
                            } 
        result  
        |> (Some << loop 0 << Ok) 
        |> function
                    | Some (Ok(State(_,_,Bucket(o))))    -> o
                                                          |> List.rev
                                                          |> List.map string
                                                          |> String.concat ""
                                                          |> Some 
                    | Some (Ok(State(_,_,Empty))) | None -> None
                    | Some(Error(msg)) -> Some msg
                