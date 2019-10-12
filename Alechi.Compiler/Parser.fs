module Alechi.Compiler.Parse


open Alechi.Compiler.Utils
open Alechi.Compiler.Ast
open FParsec


let ws = spaces
let ws1 = spaces1


let TRUE = stringReturn "true" (Constant.Bool true)
let FALSE = stringReturn "false" (Constant.Bool false)
let UNIT = stringReturn "()" Constant.Unit
let INTEGER = pint64 |>> Constant.Int
let FLOAT = pfloat |>> Constant.Float


let ident = pstring "ident"


let constant =
    TRUE <|> FALSE <|> UNIT <|> INTEGER <|> FLOAT


let constantExpr = constant |>> Expression.Constant


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


let simpleExpression = constantExpr <|> ifExpr


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
