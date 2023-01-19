let evalFile env f =
  let ic = open_in f in
  try (let evaluated = ic |> Rvg.CharStream.inputChannelToSeq
      |> Rvg.Ast.parseTopLevel
      |> Rvg.Eval.evalExpr env |> fst
    in let result = Rvg.Eval.Environment.add
      (f |> Filename.basename |> Filename.remove_extension)
      evaluated env
    in close_in ic; result)
  with e ->
    close_in_noerr ic;
    raise e

let () =
  ignore(Array.fold_left evalFile Rvg.Eval.Environment.empty (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)))
