module internal Alechi.Tests.Utils


open FParsec


let run = CharParsers.run


let toSucc: CharParsers.ParserResult<_, _> -> obj = function
    | Success (out, _, _) -> out :> obj
    | otherwise -> otherwise :> obj


let isErrMsg: CharParsers.ParserResult<_, _> -> obj = function
    | Failure _ -> true :> obj
    | otherwise -> otherwise :> obj


// exception ParseFailureException of string * ParserError


let succeeds: CharParsers.ParserResult<_, _> -> 'a = function
    | Success (out, _, _) -> out
    | Failure (msg, _, _) -> failwith msg
    // | Failure (msg, err, _) -> raise (ParseFailureException (msg, err))


let fails: CharParsers.ParserResult<_, _> -> _ = function
    | Failure (_, err, _) -> err.Messages.Head
    | Success (out, _, _) -> failwith (sprintf "%A" out)
