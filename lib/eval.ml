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
        else raise (EvalFail ("Expected " ^ (string_of_int nparams) ^ " arguments but got " ^ (string_of_int nargs) ^ " arguments: " ^ (String.concat ", " (List.map Ast.exprToString args))))

let rec evalExpr env e = match e with
  | Ast.Name (str, _) -> if Environment.mem str env
    then evalExpr env (Environment.find str env)
    else raise (EvalFail ("Unbound name: " ^ str))
  | Ast.Var (_, _) -> raise (EvalFail (
    "A parameter is not an expression, but tried to evaluate "
    ^ (Ast.exprToString e)))
  | Ast.ParsedAsm (_, _) -> e, env
  | Ast.Asm (asm, meta) -> Ast.ParsedAsm (
      (Assembly.parse Assembly.empty (expectAsm env) asm),
      meta
    ), env
  | Ast.Template (tem, meta) ->
    let exprs', env' = evalExprListInOrder env tem in
    let tem = Ast.Template (exprs', meta) in
    ParsedAsm (Ast.exprToParsedAsm (expectAsm env') tem), env'
  | Ast.Lam ({params; lbody; env = prevEnv }, meta) -> (
    if Environment.is_empty prevEnv
      then (Ast.Lam ({ params; lbody; env }, meta)), env
      else e, env)
  | Ast.Mu _ -> e, env
  | Ast.LamApplication ({ lam; args }, _) -> (
    let args', env' = evalExprListInOrder env args in
    (match lam with
    | Name (s, _) when Std.E.mem s Std.functions ->
      (Std.E.find s Std.functions) args', env'
    | _ ->
    let lam', _ = evalExpr env' lam in
    match lam' with
    | Lam ({ params; lbody; env = env'' }, _) ->
      let env''' = bindNames env'' (List.map fst params) args' in
      evalSequence env''' lbody, env
    | Mu ({ mparams; mbody }, _) ->
      let env'' = bindNames env' (List.map fst mparams) args' in
      evalSequence env'' mbody, env
    | e -> raise (EvalFail ("Expected Lam but got " ^ Ast.exprToString e))))
  | Ast.Def (define, _) -> (let evaluated = evalSequence env define.dvalue in
    evaluated, bindNames env [fst define.dname] [evaluated])
and evalExprListInOrder env exprList =
  let exprs, env = List.fold_left
    ( fun (exprs, env) expr ->
      let expr', env' = evalExpr env expr in
        expr' :: exprs, env'
    ) ([], env) (List.rev exprList)
  in exprs, env
and evalSequence env exprList =
  let exprs', _ = evalExprListInOrder env exprList in
  let head = List.hd exprs' in
  head
and expectAsm env name description =
(if Environment.mem name env
  then match evalExpr env (Environment.find name env) with
  | Ast.Asm (num, meta), _ -> num, meta.r
  | Ast.ParsedAsm (pasm, meta), _ -> (match pasm with
    | Fragment f -> f, meta.r
    | _ -> raise (Assembly.AsmParseFail ("Expected assembly fragment, but got assembly block")))
  | bad, _ -> raise (Assembly.AsmParseFail (
    description ^ " expected, but found expression " ^ Ast.exprToString bad))
  else Assembly.failWithNoBinding name)

let printAsm str = str |> Assembly.parse Assembly.empty (expectAsm Environment.empty)
  |> Assembly.asmToString |> print_endline

let%expect_test _ =
  printAsm {|
  add t0 t1 t2
  addi a0 a1 0x66
  lbu s4 12(t6)
  beq a5 t3 END
  jal ra END3

  jalr zero t4 END2
  |};
  [%expect{|
    Block(, FinishedBlock(RType(add, temp-t0, temp-t1, temp-t2)
    IArith(addi, temp-a0, temp-a1, 0x66)
    Load(lbu, save-s4, temp-t6, 12)
    Branch(beq, temp-a5, temp-t3, END)
    Jal(jal, Ra, END3)
    Jalr(jalr, Zero, temp-t4, END2)),   ) |}]

let printReducedAst text =
  text |> Ast.getAst |> evalExpr Environment.empty |> fst |> Ast.exprToString
  |> print_endline

let%expect_test _ = printReducedAst "[lam [] \"\" ]";
  [%expect{| Lam(params=[], lbody=Name("")) |}]

let%expect_test _ = printReducedAst "[[lam [] {}]]";
  [%expect{| ParsedAsm(Fragment()) |}]

let%expect_test _ = printReducedAst "[[lam [(x)] {}] {} ]";
  [%expect{| ParsedAsm(Fragment()) |}]

let%expect_test _ = print_endline (Ast.exprToString (Ast.ParsedAsm (Ast.exprToParsedAsm (expectAsm Environment.empty) (Ast.Asm (" 12 ", Ast.metaEmpty)))));
  [%expect {| ParsedAsm(Fragment( 12 )) |}]

let%expect_test _ = Ast.printAst {|
  [[lam [(x)] {
    [[lam [(x)] {
      addi zero zero x
    }] x]
    x
  }] { 12 }]
  |};
  [%expect {|
    LamApplication(lam=Lam(params=[Var(name=x)], lbody=Template(Asm(
        )LamApplication(lam=Lam(params=[Var(name=x)], lbody=Template(Asm(
          addi zero zero x
        ))), args=(Name(x)))Asm(
        x
      ))), args=(Template(Asm( 12 )))) |}]


let%expect_test _ = printReducedAst {|
    [[lam [(x)] {
      addi t0 t1 x
    }] { 12 }]
  |};
  [%expect {| ParsedAsm(FinishedBlock(IArith(addi, temp-t0, temp-t1,  12 ))) |}]

let%expect_test _ = printReducedAst {|
  [[lam [(x)] {
    [[lam [(x)] {
      addi zero zero x
    }] x]
    slli t1 t2 x
  }] { 12 }]
  |};
  [%expect {|
    ParsedAsm(Block(
        , FinishedBlock(IArith(addi, Zero, Zero,  12 )
    IArith(slli, temp-t1, temp-t2,  12 )), )) |}]

let%expect_test _ = printReducedAst {|
    [[lam [(x)]
      [def (a) x]
      {add t0 t1 t2}
      a
    ] {addi zero zero 0} ]
  |};
  [%expect{| ParsedAsm(FinishedBlock(IArith(addi, Zero, Zero, 0))) |}]

let%expect_test _ = printReducedAst {|
    [
      [
        [lam [(x)]
          [def (bound_in_closure) {t0}]
          [lam [(y)] {sub bound_in_closure x y}]]
        {t1}]
      {t2}]
  |};
  [%expect{| ParsedAsm(FinishedBlock(RType(sub, temp-t0, temp-t1, temp-t2))) |}]

let%expect_test _ = printReducedAst {|
    [[lam []
      [def (x) {t0}]
      [def (m) [[lam [(x)] [mu [] {lw x 0(a0)}]] {a0}]]
      [m]]]
  |};
  [%expect{| ParsedAsm(FinishedBlock(Load(lw, temp-t0, temp-a0, 0))) |}]


let%expect_test _ = printReducedAst {|
  [[lam []
    [def (x) {t0}]
    [def (m) [
      [lam [(z)]
        [lam [] {lw z 0(a0)}]]
      {a0}]]
    [m]]]
  |};
  [%expect{| ParsedAsm(FinishedBlock(Load(lw, temp-a0, temp-a0, 0))) |}]

let%expect_test _ = (try
  printReducedAst {|
  [[
    [lam [(true) (false)] true]
    {help}
    [lam [] [fail {help}]]]]
  |} with
    | Std.AssertionFail s -> print_string s
    | EvalFail s -> print_string s);
  [%expect{| Expected Lam but got ParsedAsm(Fragment(help)) |}]


let%expect_test _ = (try
  printReducedAst {|
  [[
    [lam [(true) (false)] false]
    {help}
    [lam [] [fail {help}]]]]
  |} with
    | Std.AssertionFail s -> print_string s
    | EvalFail s -> print_string s);
  [%expect{| Assertion failed: line 4, col 18 to line 4, col 24 |}]
