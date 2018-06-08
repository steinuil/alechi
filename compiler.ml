open Sexplib.Type


let failwithf fmt =
  Printf.ksprintf failwith fmt


(* Types *)

type type_expr =
  | Basic of string


type constant =
  | IntConstant of string
  | FloatConstant of string
  | EnumConstant of string
  | CharConstant of string
  | StrLiteral of string


type expression =
  | Constant of constant
  | Variable of string
  | Funcall of string * expression list


type statement =
  | DeclStmt of string * type_expr * expression option
  | SetStmt of string * expression
  | Ignore of expression
  | Label of string
  | Goto of string
  | IfStmt of expression * statement list * statement list
  | Return of expression


type declaration =
  | Include of string
  | IncludeRel of string
  | Typedef of string * type_expr
  | Struct of string * (string * type_expr) list
  | Union  of string * (string * type_expr) list
  | Decl of string * type_expr * expression option
  | Proc of string * (string * type_expr) list * type_expr * statement list



let rec parse_type_expr = function
  | Atom t ->
    Basic t

  | _ ->
    failwith "invalid type"


let rec parse_expr = function
  | List [Atom "'int"; Atom n] ->    Constant (IntConstant n)
  | List [Atom "'float"; Atom f] ->  Constant (FloatConstant f)
  | List [Atom "'enum"; Atom e] ->   Constant (EnumConstant e)
  | List [Atom "'char"; Atom c] ->   Constant (CharConstant c)
  | List [Atom "'string"; Atom s] -> Constant (StrLiteral s)

  | List (Atom fn :: args) ->
    let args = List.map parse_expr args in
    Funcall (fn, args)

  | Atom var ->
    Variable var

  | _ ->
    failwith "invalid expression"


let rec parse_statement = function
  | List [Atom "decl"; Atom n; t] ->
    let t = parse_type_expr t in
    DeclStmt (n, t, None)

  | List [Atom "decl"; Atom n; t; expr] ->
    let t = parse_type_expr t in
    let expr = parse_expr expr in
    DeclStmt (n, t, Some expr)

  | List [Atom "set!"; Atom n; expr] ->
    let expr = parse_expr expr in
    SetStmt (n, expr)

  | List [Atom "ignore"; expr] ->
    let expr = parse_expr expr in
    Ignore expr

  | List [Atom "label"; Atom lbl] ->
    Label lbl

  | List [Atom "goto"; Atom lbl] ->
    Goto lbl

  | List [Atom "if"; expr; List t_branch] ->
    let expr = parse_expr expr in
    let t_branch = List.map parse_statement t_branch in
    IfStmt (expr, t_branch, [])

  | List [Atom "if"; expr; List t_branch; List f_branch] ->
    let expr = parse_expr expr in
    let t_branch = List.map parse_statement t_branch in
    let f_branch = List.map parse_statement f_branch in
    IfStmt (expr, t_branch, f_branch)

  | List [Atom "return"; expr] ->
    let expr = parse_expr expr in
    Return expr

  | List (Atom x :: _) ->
    failwithf "invalid statement: %s" x

  | _ ->
    failwith "invalid statement"


let parse_struct_item = function
  | List [Atom name; t] ->
    let t = parse_type_expr t in
    (name, t)

  | _ ->
    failwith "invalid struct item"


let rec parse_declaration = function
  | List [Atom "include"; Atom str] ->
    Include str

  | List [Atom "include_rel"; Atom str] ->
    IncludeRel str

  | List [Atom "typedef"; Atom name; t] ->
    let t = parse_type_expr t in
    Typedef (name, t)

  | List (Atom "struct" :: Atom name :: struct_items) ->
    let struct_items = List.map parse_struct_item struct_items in
    Struct (name, struct_items)

  | List (Atom "union" :: Atom name :: struct_items) ->
    let struct_items = List.map parse_struct_item struct_items in
    Union (name, struct_items)

  | List [Atom "decl"; Atom name; t] ->
    let t = parse_type_expr t in
    Decl (name, t, None)

  | List [Atom "decl"; Atom name; t; expr] ->
    let t = parse_type_expr t in
    let expr = parse_expr expr in
    Decl (name, t, Some expr)

  | List (Atom "proc" :: Atom name :: List args :: return_t :: stmts) ->
    let args = List.map parse_struct_item args in
    let return_t = parse_type_expr return_t in
    let stmts = List.map parse_statement stmts in
    Proc (name, args, return_t, stmts)

  | _ ->
    failwith "invalid declaration"



let%test_unit "parse" =
  let f = Sexplib.Sexp.load_sexps "hello.als" in
  let _ = List.map parse_declaration f in
  ()


(* Generate *)
(*
let str_typ = function
  | Basic t -> t

let str_proc_args = function
  | [] -> "void"
  | _ -> failwith "not supported rn"

let str_const = function
  | IntConstant n | FloatConstant n | CharConstant n | EnumConstant n ->
    n
  | StrLiteral s ->  "\"" ^ String.escaped s ^ "\""


let str_expr = function
  | Constant c -> str_const c

let generate out =
  let fe fmt = Printf.fprintf out (fmt ^^ "\n") in
  let fo fmt = Printf.fprintf out fmt in
  let po s = output_string out s; output_char out '\n' in
  List.iter (function
    | Import header ->
      fe "#include \"%s\"" header
    | Proc (name, args, typ, stmts) ->
      fo "%s %s(%s){" (str_typ typ) name
        (str_proc_args args);
      stmts |> List.iter (function
        | Return e ->
          fo "return %s;" (str_expr e)
        | Funcall (fn, args) ->
          fo "%s(%s);" fn
            (String.concat "," (List.map str_expr args))
      );
      po "}"
  )

*)
