module Alechi.Compiler.Parse

open Alechi.Compiler.Ast
open FParsec


type UserData = unit


val keywords: string list

val ident: Parser<Ident, UserData>

val longIdent: Parser<LongIdent, UserData>

val constant: Parser<Constant, UserData>

val expression: Parser<Expression, UserData>

val topLevel: Parser<TopLevel, UserData>
