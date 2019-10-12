namespace Alechi.Compiler.Ast


type Ident = string


type LongIdent = Ident


[<RequireQualifiedAccess>]
type Constant =
    | Int of int64
    | Float of float
    // | Char of char
    | String of string
    | Bool of bool
    | Unit


[<RequireQualifiedAccess>]
type Expression =
    | Identifier of LongIdent
    | Constant of Constant
    | Let of Ident * Expression * Expression
    | Apply of Expression * Expression list
    | If of Expression * Expression * Expression option
    | While of Expression * Expression
    | Tuple of Expression list


type Arguments = Ident list


[<RequireQualifiedAccess>]
type TopLevel =
    | Import of string
    | Proc of Ident * Arguments * Expression
