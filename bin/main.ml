let evalFile env f =
  let ic = open_in f in
  try (let evaluated = ic |> Rvg.CharStream.inputChannelToSeq
      |> Rvg.Ast.parseTopLevel Rvg.Std.std
      |> Rvg.Eval.evalExpr env |> fst
    in let result = Rvg.Eval.Environment.add
      (f |> Filename.basename |> Filename.remove_extension)
      evaluated env
    in close_in ic; result)
  with e ->
    close_in_noerr ic;
    raise e

let () =
  try ignore(
    Array.fold_left
    (fun env f -> if String.ends_with ~suffix:".rvg" f then evalFile env f else env)
    Rvg.Eval.Environment.empty
    (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))
  with e ->
    (match e with
    | Rvg.Ast.ParseFail (s, (p: Rvg.CharStream.position)) -> print_endline (
      "Line " ^ (string_of_int (p.zeroBasedLine + 1)) ^ ", col " ^ (string_of_int (p.zeroBasedCol + 1)) ^ ": " ^ s)
    | Rvg.Eval.EvalFail (s, r) -> print_endline (
      (Rvg.CharStream.rangeToString r) ^ ": " ^ s
    )
    | Rvg.Std.AssertionFail s -> print_endline s
    | _ -> raise e)
