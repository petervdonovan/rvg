let evaluate env f s =
  let evaluated = s |> Rvg.Ast.parseTopLevel Rvg.Std.stdFun |> Rvg.Eval.evalExpr env |> fst in
  Rvg.Eval.Environment.add (f |> Filename.basename |> Filename.remove_extension) evaluated env

let evalFile env f =
  let s, ic = Rvg.CharStream.fromFile f in
  try
    let result = evaluate env f s in
    close_in ic ; result
  with e -> close_in_noerr ic ; raise e

let evaluate env fname =
  let parts = String.split_on_char '=' fname in
  if List.length parts > 1 then
    let name, contents = (List.hd parts, List.tl parts) in
    evaluate env name (Rvg.CharStream.fromString name (String.concat "=" contents))
  else evalFile env fname

let printMessageRange s r = print_endline (Rvg.CharStream.rangeToString r ^ ": " ^ s)
let printRange r = print_endline (Rvg.CharStream.rangeToString r)

let () =
  try
    ignore
      (Array.fold_left
         (fun env f -> evaluate env f)
         Rvg.Std.std
         (let nskipped = 1 + Rvg.SideEffects.nargs in
          Array.sub Sys.argv nskipped (Array.length Sys.argv - nskipped) ) )
  with e -> (
    match e with
    | Rvg.Ast.ParseFail (s, (p : Rvg.CharStream.position)) ->
        printMessageRange s {startInclusive= p; endExclusive= p}
    | Rvg.Eval.EvalFail (s, r) ->
        print_endline s;
        r |> List.rev |> List.iter printRange
    | Rvg.Std.IllegalArgument (s, r) ->
        printMessageRange s r
    | Rvg.Std.WrongNumberOfArgs (s, r) ->
        printMessageRange s r
    | _ ->
        raise e )
