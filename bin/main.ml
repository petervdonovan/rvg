let evalFile env f =
  let s, ic = Rvg.CharStream.fromFile f in
  try (let evaluated = s
      |> Rvg.Ast.parseTopLevel Rvg.Std.stdFun
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
    (fun env f -> evalFile env f)
    Rvg.Std.std
    (let nskipped = 1 + Rvg.SideEffects.nargs in Array.sub Sys.argv nskipped (Array.length Sys.argv - nskipped)))
  with e ->
    (match e with
    | Rvg.Ast.ParseFail (s, (p: Rvg.CharStream.position)) -> print_endline (
      "Line " ^ (string_of_int (p.zeroBasedLine + 1)) ^ ", col " ^ (string_of_int (p.zeroBasedCol + 1)) ^ ": " ^ s)
    | Rvg.Eval.EvalFail (s, r) -> print_endline (
      (Rvg.CharStream.rangeToString r) ^ ": " ^ s
    )
    | Rvg.Std.AssertionFail s -> print_endline s
    | _ -> raise e)
