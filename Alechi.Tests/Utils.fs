module internal Alechi.Tests.Utils


open FParsec


let run = CharParsers.run


let succeeds: CharParsers.ParserResult<_, _> -> 'a = function
    | Success (out, _, _) -> out
    | Failure (msg, _, _) -> failwith msg


let fails: CharParsers.ParserResult<_, _> -> _ = function
    | Failure (_, err, _) -> err.Messages.Head
    | Success (out, _, _) -> failwith (sprintf "%A" out)
