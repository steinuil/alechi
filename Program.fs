open System
open System.IO

let lexStuff text =
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromTextReader text
    let rec lexAll acc =
        try
            let x = Lexer.lex lexbuf
            lexAll (x :: acc)
        with _ ->
            acc


    lexAll [] |> List.rev


let parse text =
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromTextReader text
    let program = Parser.initial Lexer.lex lexbuf
    program

[<EntryPoint>]
let main = function
    | [|fname|] ->
        use textReader = new StreamReader(fname)
        lexStuff textReader |> printfn "%A"
        use textReader = new StreamReader(fname)
        parse textReader |> printfn "%A"
        0
    | _ ->
        failwith "no inputs given"
