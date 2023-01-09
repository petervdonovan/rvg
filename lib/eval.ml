module Environment = Map.Make(String)

exception EvalFail of string

let bindNames env params args =
  let nparams = List.length params in
    let nargs = List.length args in
      if nparams = nargs
        then List.fold_left2
          (fun env param arg ->
            let p : Ast.var = param in
              Environment.add p.name arg env)
          env params args
        else raise (EvalFail ("Expected " ^ (string_of_int nparams) ^ " arguments but got " ^ (string_of_int nargs) ^ " arguments." ))

let rec evalExpr env e = match e with
  | Ast.Name str -> if Environment.mem str env
    then evalExpr env (Environment.find str env)
    else raise (EvalFail ("Unbound name: " ^ str))
  | Ast.Var v -> raise (EvalFail (
    "A parameter is not an expression, but tried to evaluate "
    ^ (Ast.exprToString (Ast.Var v))))
  | Ast.Asm asm -> Ast.Asm asm, env
  | Ast.Template tem ->
    let flattened = List.map (
        fun e -> match e with
          | Ast.Template exprs -> exprs
          | _ -> [e]
      ) tem |> List.concat
    in
      let exprs', env' = evalExprListInOrder env flattened
        in Ast.Template exprs', env'
  (* | Ast.Template tem -> Ast.Template (List.map evalExpr env (List.concat (
      fun e -> match e with
      | Ast.Template exprs -> exprs
      | _ -> [e]
    ) tem)) *)
  | Ast.Lam lam -> Ast.Lam lam, env
  | Ast.LamApplication { lam; args } ->
    let lam', env' = evalExpr env lam in
      let args', _ = evalExprListInOrder env' args in
        match lam' with
        | Lam { params; value; env } -> evalExpr (bindNames env params args') value
        | e -> raise (EvalFail ("Expected Lam but got " ^ Ast.exprToString e))
and evalExprListInOrder env exprList = List.fold_left (
  fun (exprs, env) expr ->
    let expr', env' = evalExpr env expr in
      expr' :: exprs, env') ([], env) exprList

let evalProgram exprs = exprs |> evalExprListInOrder Environment.empty |> fst

let printReducedAst text =
  text |> Ast.getAst |> evalProgram |> List.map Ast.exprToString
  |> List.iter print_endline

let%expect_test _ = printReducedAst "[lam [] \"\" ]";
  [%expect{| Lam(params=[], value=Name("")) |}]

let%expect_test _ = printReducedAst "[[lam [] {}]]";
  [%expect{| Template(Asm()) |}]

let%expect_test _ = printReducedAst "[[lam [(x)] {}] {} ]";
  [%expect{| Template(Asm()) |}]
