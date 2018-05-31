let () =
  match Sys.argv with
  | [| _; file |] ->
    let f = Sexplib.Sexp.load_sexps "hello.als" in
    let decls = List.map Compiler.parse_declaration f in
    let out = open_out file in
    begin try
      Compiler.generate out decls;
      close_out out
    with exn ->
      close_out out;
      raise exn
    end

  | _ ->
    ()
