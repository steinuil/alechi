module Alechi.Compiler.Parse


open Alechi.Compiler.Utils
open Alechi.Compiler.Ast
open FParsec


type UserData = unit


let ws = spaces
let ws1 = spaces1


let keywords = [
    "true"
    "false"
    "let"
    "if"
    "else" 
    "proc"
]


let identChars = Seq.append ['a' .. 'z'] ['_']


let ident: Parser<Ident, _> =
    let ident = identChars |> anyOf |> many1Chars
    let checkNotKeyword id =
        if List.contains id keywords then
            fail <| sprintf "ident %s is a keyword" id
        else
            preturn id
    ident >>= checkNotKeyword |> attempt


let longIdent: Parser<LongIdent, _> = ident


let cString =
    skipChar '"' >>. pstring "string" .>> skipChar '"'
    |>> Constant.String


let constant =
    let cTrue = stringReturn "true" (Constant.Bool true)
    let cFalse = stringReturn "false" (Constant.Bool false)
    let cUnit = stringReturn "()" Constant.Unit
    let cInteger = pint64 |>> Constant.Int
    let cFloat = pfloat |>> Constant.Float
    cTrue <|> cFalse <|> cUnit <|> cInteger <|> cFloat <|> cString


let expression, expressionRef = createParserForwardedToRef()


let eIf =
    let cond = skipString "if" >>. ws >>. expression .>> ws
    let eBlock = skipChar '{' >>. ws >>. expression .>> ws .>> skipChar '}' .>> ws
    let elseBlock = skipString "else" >>. ws >>. eBlock
    Expression.If |> uncurry3 |> pipe3 cond eBlock (opt elseBlock)


let eLetUnit =
    let before = expression .>> ws .>> skipChar ';' .>> ws
    Expression.LetUnit |> uncurry2 |> pipe2 before expression


let eLet =
    let bind = skipString "let" >>. ws1 >>. ident .>> ws .>> skipChar '=' .>> ws
    let body = expression .>> ws .>> skipChar ';' .>> ws
    Expression.Let |> uncurry3 |> pipe3 bind body expression


let eSimple =
    let eConstant = constant |>> Expression.Constant
    let eLongIdent = longIdent |>> Expression.Identifier
    eConstant <|> eLongIdent <|> eIf


let eBlock =
    skipChar '{' >>. ws >>. expression .>> ws .>> skipChar '}'


// let eComplexUnwrapped = eLet <|> eLetUnit


// let eComplex =
//     skipChar '{' >>. ws >>. eComplexUnwrapped .>> ws .>> skipChar '}'


do expressionRef := eSimple <|> eLet <|> eBlock


let pName =
    skipString "proc" >>. ws1 >>. ident .>> ws


let arguments =
    let argument = ws >>. ident .>> ws
    skipChar '(' >>. sepBy argument (skipChar ',') .>> skipChar ')' .>> ws


let pBody =
    skipChar '{' >>. ws >>. expression .>> ws .>> skipChar '}'


let proc =
    pipe3 pName arguments pBody (uncurry3 TopLevel.Proc)


let topLevel: Parser<_, UserData> =
    proc
