module internal Alechi.Tests.Utils


open FParsec


let run = CharParsers.run


let toSucc: CharParsers.ParserResult<_, _> -> obj = function
    | Success (out, _, _) -> out :> obj
    | otherwise -> otherwise :> obj


let isErrMsg: CharParsers.ParserResult<_, _> -> obj = function
    | Failure _ -> true :> obj
    | otherwise -> otherwise :> obj
