module Ast


type Statement =
  | Let of string * Statement list option
  | While of Statement list * Statement list
  | Expr of Expression

and Expression =
  | IfElse of Statement list * Statement list * Statement list option
  | FunCall of string * Statement list list
  | Ident of string
  | String of string
  | Number of string
  | Array of Statement list list


type Decl =
  | Proc of string * string list * Statement list