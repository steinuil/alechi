open Sexplib.Type


let failwithf fmt =
  failwith (Printf.sprintf fmt)


(* Types *)
type typ =
  | Basic of string


type expression =
  | Number of string
  | String of string


type statement =
  | Funcall of string * expression list
  | Return of expression


type args = (string * typ) list


type decl =
  | Import of string
  | Proc of string * args * typ * statement list


(* Parsing *)
let parse_type = function
  | Atom t ->
    Basic t

  | _ ->
    failwith "unsupported kind of type"


let parse_expr = function
  | Atom n ->
    Number n

  | List [Atom "'string"; Atom s] ->
    String s

  | _ ->
    failwith "expression not supported"

let parse_arg = function
  | List [Atom name; typ] ->
    let typ = parse_type typ in
    name, typ

  | _ ->
    failwith "not an argument"


let parse_statement = function
  | List [Atom "return"; expr] ->
    let expr = parse_expr expr in
    Return expr

  | List (Atom fn :: args) ->
    let args = List.map parse_expr args in
    Funcall (fn, args)

  | _ ->
    failwith "unsupported statement"


let parse_declaration = function
  | List [Atom "import"; Atom name] ->
    Import name

  | List (Atom "proc" :: Atom name :: List args :: return_t :: stmts) ->
    let args = List.map parse_arg args in
    let return_t = parse_type return_t in
    let stmts = List.map parse_statement stmts in
    Proc (name, args, return_t, stmts)

  | _ ->
    failwith "not a declaration"


(* Generate *)
let str_typ = function
  | Basic t -> t

let str_proc_args = function
  | [] -> "void"
  | _ -> failwith "not supported rn"

let str_expr = function
  | Number n -> n
  | String s -> "\"" ^ String.escaped s ^ "\""

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


  (*
let%test_unit "test" =
  let f = Sexp.load_sexps "hello.als" in
  let decls = List.map parse_declaration f in
  generate stdout decls
  *)
