namespace Alechi.Compiler


module C =
    open FParsec

    let ctrue = stringReturn "true" (Ast.Bool true)
    let cfalse = stringReturn "false" (Ast.Bool false)

    let cnumber = pfloat |>> Ast.Number
    