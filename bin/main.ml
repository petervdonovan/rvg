let file = "example.dat"

let () =

  (* Read file and display the first line *)
  let ic = open_in file in
  try
    print_newline ();
    print_string (List.fold_left (^) "" (List.map Rvg.Ast.exprToString (Rvg.Ast.parseTopLevel ic [])));
    print_newline ();
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e
