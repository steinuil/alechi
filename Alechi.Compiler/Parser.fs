module Alechi.Compiler.Parse


open Alechi.Compiler.Utils
open Alechi.Compiler.Ast
open FParsec


let TRUE = stringReturn "true" (Constant.Bool true)
let FALSE = stringReturn "false" (Constant.Bool false)
let UNIT = stringReturn "()" Constant.Unit
let INTEGER = pint64 |>> Constant.Int
let FLOAT = pfloat |>> Constant.Float


let ident = pstring "ident"


let constant =
    TRUE <|> FALSE <|> UNIT <|> INTEGER <|> FLOAT


let expression, expressionRef = createParserForwardedToRef()


let exprLet =
    pipe2
        (skipString "let" >>. spaces1 >>. ident
        .>> spaces .>> skipChar '=' .>> spaces)
        expression
        (uncurry2 Expression.Let)


do expressionRef :=
    (constant |>> Expression.Constant)
    <|> exprLet


let procName =
    skipString "proc" >>. spaces1 >>. ident .>> spaces


let argument =
    spaces >>. ident .>> spaces


let arguments =
    skipChar '(' >>. sepBy argument (skipChar ',') .>> skipChar ')' .>> spaces


let procBody =
    skipChar '{' >>. spaces >>. expression .>> spaces .>> skipChar '}'


let proc: Parser<_, unit> =
    pipe3 procName arguments procBody (uncurry3 TopLevel.Proc)
