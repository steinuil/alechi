module Alechi.Compiler.Lexer

open FSharp.Text.Lexing

val lex : LexBuffer<char> -> Parser.token