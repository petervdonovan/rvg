let file = "temp.dat"

let () =

  (* Read file and display the first line *)
  let ic = open_in file in
  try
    print_newline ();
    List.iter print_endline (List.map Rvg.Ast.exprToString (
      ( Rvg.Ast.parseTopLevel [] (Rvg.Ast.inputChannelToSeq ic))
    ));
    print_newline ();
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e
