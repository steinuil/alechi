module Alechi.Compiler.Parse


open Alechi.Compiler.Utils
open Alechi.Compiler.Ast
open FParsec


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


let ident =
    let p =
        Seq.append ['a' .. 'z'] ['_']
        |> anyOf
        |> many1Chars
    let checkNotKeyword id =
        if List.contains id keywords then
            fail <| sprintf "ident %s is a keyword" id
        else
            preturn id
    p >>= checkNotKeyword |> attempt


let cString = skipChar '"' >>. pstring "string" .>> skipChar '"' |>> Constant.String


let constant =
    let cTrue = stringReturn "true" (Constant.Bool true)
    let cFalse = stringReturn "false" (Constant.Bool false)
    let cUnit = stringReturn "()" Constant.Unit
    let cInteger = pint64 |>> Constant.Int
    let cFloat = pfloat |>> Constant.Float
    cTrue <|> cFalse <|> cUnit <|> cInteger <|> cFloat <|> cString


let expression, expressionRef = createParserForwardedToRef()


let ifExpr =
    let cond = skipString "if" >>. ws >>. expression .>> ws
    let thenExpr = skipChar '{' >>. ws >>. expression .>> ws .>> skipChar '}' .>> ws
    let elseExpr = skipString "else" >>. ws >>. thenExpr
    Expression.If
    |> uncurry3
    |> pipe3 cond thenExpr (opt elseExpr)


let exprLet =
    let bind = skipString "let" >>. ws1 >>. ident .>> ws .>> skipChar '=' .>> ws
    let body = expression .>> ws .>> skipChar ';' .>> ws
    Expression.Let
    |> uncurry3
    |> pipe3 bind body expression


let simpleExpression =
    let constantExpr = constant |>> Expression.Constant
    let identExpr = ident |>> Expression.Identifier
    constantExpr <|> identExpr <|> ifExpr


do expressionRef := simpleExpression <|> exprLet


let procName =
    skipString "proc" >>. ws1 >>. ident .>> ws


let arguments =
    let argument = ws >>. ident .>> ws
    skipChar '(' >>. sepBy argument (skipChar ',') .>> skipChar ')' .>> ws


let procBody =
    skipChar '{' >>. ws >>. expression .>> ws .>> skipChar '}'


let proc: Parser<_, unit> =
    pipe3 procName arguments procBody (uncurry3 TopLevel.Proc)
