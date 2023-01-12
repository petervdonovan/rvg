let file = "temp.dat"

let () =

  (* Read file and display the first line *)
  let ic = open_in file in
  try
    print_newline ();
    (Rvg.Ast.parseTopLevel [] (Rvg.ParseUtil.inputChannelToSeq ic)) |> Rvg.Eval.evalProgram |> List.map Rvg.Ast.exprToString |> List.iter print_endline;

    (* List.iter print_endline (List.map Rvg.Ast.exprToString (
      ( Rvg.Ast.parseTopLevel [] (Rvg.ParseUtil.inputChannelToSeq ic))
    )); *)
    print_newline();
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e
