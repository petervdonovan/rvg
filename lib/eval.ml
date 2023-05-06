module Environment = Map.Make (String)

exception EvalFail of string * CharStream.range list

exception AssertionFail of string * CharStream.range

let rec evalExpr env e =
  let _, (meta : Ast.metadata) = e in
  try evalExprRec env e with
  | EvalFail (s, r) ->
      raise (EvalFail (s, meta.r :: r))
  | AssertionFail (s, r) ->
      raise (EvalFail (s, [r]))

and evalExprRec env e =
  match e with
  | Ast.Name str, (meta : Ast.metadata) ->
      if Environment.mem str env then (
        let definition : Ast.expr = Environment.find str env in
        SideEffects.reportResolvedName meta.r definition ;
        evalExpr env definition )
      else raise (EvalFail ("Unbound name: " ^ str, [Ast.rangeOf e]))
  | Ast.Integer _, _ ->
      (e, env)
  | Ast.Var _, _ ->
      raise
        (EvalFail
           ( "A parameter is not an expression, but tried to evaluate " ^ Ast.exprToString e
           , [Ast.rangeOf e] ) )
  | Ast.ParsedAsm _, _ ->
      (e, env)
  | Ast.Asm _, _ ->
      (e, env)
  | Ast.Template (tem, prevEnv), meta ->
      let exprs', env' = evalExprListInOrder env (List.rev tem) in
      let exprs' = mergeExprs exprs' in
      let tem =
        (Ast.Template (exprs', if Environment.is_empty prevEnv then env else prevEnv), meta)
      in
      (tem, env')
  | Ast.Lam {params; lbody; env= prevEnv; f}, meta ->
      if Environment.is_empty prevEnv then ((Ast.Lam {params; lbody; env; f}, meta), env)
      else (e, env)
  | Ast.LamApplication {lam; args}, meta -> (
      let lam', env' = evalExpr env lam in
      match lam' with
      | Lam {params; lbody; env= env''; f}, _ ->
          let args' = parseArgs params args meta.r in
          let args'', _ = evalExprListInOrder env' args' in
          (f args'' params lbody env'' env' (evalSequence meta.r) meta.r, env')
      | e ->
          raise (EvalFail ("Expected Lam but got " ^ Ast.exprToString e, [Ast.rangeOf e])) )
  | Ast.Def define, meta ->
      let evaluated = (evalSequence meta.r) env define.dvalue in
      (evaluated, bindNames env [fst define.dname] [evaluated] (Ast.rangeOf e))

and evalExprListInOrder env exprList =
  let exprs, env =
    List.fold_left
      (fun (exprs, env) expr ->
        let expr', env' = evalExpr env expr in
        (expr' :: exprs, env') )
      ([], env) (List.rev exprList)
  in
  (exprs, env)

and evalSequence r env exprList =
  if List.length exprList == 0 then raise (EvalFail ("Expected sequence, but got nothing", [r]))
  else
    let exprs', _ = evalExprListInOrder env exprList in
    let head = List.hd exprs' in
    head

and bindNames env params args r =
  let nparams = List.length params in
  let nargs = List.length args in
  if nparams = nargs then
    List.fold_left2
      (fun env param arg ->
        let p : Ast.var = param in
        let arg', env' = applyChecks env p arg in
        Environment.add p.name arg' env' )
      env params args
  else
    raise
      (EvalFail
        ( "Expected " ^ string_of_int nparams ^ " arguments corresponding to parameters "
          ^ String.concat ", " (List.map (fun (v : Ast.var) -> v.name) params)
          ^ " but got " ^ string_of_int nargs ^ " arguments: "
          ^ String.concat ", " (List.map Ast.exprToString args)
        , [r] ) )

and parseArgs params args r =
  if List.length params = 0 then args else
  try let folder = (fun acc param arg ->
    match param with
    | Ast.Word w, _ ->
      (match arg with
      | Ast.Name a, _ when a = w -> acc
      | _, m -> raise (EvalFail ("Expected \"" ^ w ^ "\", not " ^ (arg |> Ast.exprToString), [m.r])) )
    | Ast.PVar _, _ -> arg :: acc) in
  List.fold_left2 folder [] params args |> List.rev
    with Invalid_argument _ -> raise (EvalFail ("Wrong number of arguments", [r]))


and applyChecks env (param : Ast.var) arg =
  List.fold_left
    (fun (arg, env') (check, meta) -> evalExpr env' (LamApplication {lam= check; args= [arg]}, meta))
    (arg, env) param.checks

and mergeExprs es =
  List.fold_left
    (fun acc e ->
      match e with
      | Ast.Asm s, (meta : Ast.metadata) -> (
          if List.length acc = 0 then [e]
          else
            match List.hd acc with
            | Ast.Asm s', (meta' : Ast.metadata) ->
                ( Ast.Asm (s ^ s')
                , { attrs= Ast.Attributes.union meta.attrs meta'.attrs
                  ; r=
                      ( {startInclusive= meta.r.startInclusive; endExclusive= meta'.r.endExclusive}
                        : CharStream.range ) } )
                :: List.tl acc
            | _ ->
                e :: acc )
      | Ast.Integer i, meta ->
          (Ast.Asm (string_of_int i), meta) :: acc
      | _ ->
          e :: acc )
    [] es

let getPVars params =
  List.map fst params |> List.filter_map (fun p -> match p with | Ast.PVar p' -> Some p' | _ -> None)

let lam args params lbody closure _ evalSequence r =
  let params' = getPVars params in
  let bound = bindNames closure params' args r in
  evalSequence bound lbody

let mu args params lbody closure currentEnv evalSequence r =
  let params' = getPVars params in
  let bound = bindNames currentEnv params' args r in
  evalSequence (Environment.union (fun _ a _ -> Some a) bound closure) lbody

let testStd = Environment.empty |> Environment.add "lam" lam |> Environment.add "mu" mu

let printReducedAst stdFun std text =
  text |> Ast.getAst stdFun |> evalExpr std |> fst |> Ast.exprToString |> print_endline

let%expect_test _ =
  printReducedAst testStd Environment.empty "[lam [] \"\" ]" ;
  [%expect {| E(Lam(params=[], lbody=E(Name(""), )), ) |}]

let%expect_test _ =
  printReducedAst testStd Environment.empty "[[lam [] {}]]" ;
  [%expect {| E(Template(), ) |}]

let%expect_test _ =
  printReducedAst testStd Environment.empty "[[lam [(x)] {}] {} ]" ;
  [%expect {| E(Template(), ) |}]

let%expect_test _ =
  Ast.printAst
    {|
  [[lam [(x)] {
    [[lam [(x)] {
      addi zero zero x
    }] x]
    x
  }] { 12 }]
  |} ;
  [%expect
    {|
    E(LamApplication(lam=E(Lam(params=[E(Var(name=x), )], lbody=E(Template(E(Asm(
        ), )E(LamApplication(lam=E(Lam(params=[E(Var(name=x), )], lbody=E(Template(E(Asm(
          addi zero zero x
        ), )), )), ), args=(E(Name(x), ))), )E(Asm(
        x
      ), )), )), ), args=(E(Template(E(Asm( 12 ), )), ))), ) |}]
