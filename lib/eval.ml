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
  | Ast.ParsedAsm p -> Ast.ParsedAsm p, env
  | Ast.Asm asm -> Ast.ParsedAsm (Assembly.parse Assembly.empty (expectAsm env) asm), env
  | Ast.Template tem ->
    let exprs', env' = evalExprListInOrder env tem in
    let tem = Ast.Template exprs' in
    ParsedAsm (Ast.exprToParsedAsm (expectAsm env') tem), env'
  | Ast.Lam lam -> Ast.Lam lam, env
  | Ast.LamApplication { lam; args } ->
    let lam', env' = evalExpr env lam in
      let args', _ = evalExprListInOrder env' args in
        match lam' with
        | Lam { params; value; env } -> evalExpr (bindNames env params args') value
        | e -> raise (EvalFail ("Expected Lam but got " ^ Ast.exprToString e))
and evalExprListInOrder env exprList =
  let exprs, env = List.fold_left
    ( fun (exprs, env) expr ->
      let expr', env' = evalExpr env expr in
        expr' :: exprs, env'
    ) ([], env) exprList
  in List.rev exprs, env
and expectAsm env name description =
(if Environment.mem name env
  then match evalExpr env (Environment.find name env) with
  | Ast.Asm num, _ -> num
  | Ast.ParsedAsm p, _ -> p.top
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
    RType(add, temp-t0, temp-t1, temp-t2)
    IArith(addi, temp-a0, temp-a1, 0x66)
    Load(lbu, save-s4, temp-t6, 12)
    Branch(beq, temp-a5, temp-t3, END)
    Jal(jal, Ra, END3)
    Jalr(jalr, Zero, temp-t4, END2) |}]

let evalProgram exprs = exprs |> evalExprListInOrder Environment.empty |> fst

let printReducedAst text =
  text |> Ast.getAst |> evalProgram |> List.map Ast.exprToString
  |> List.iter print_endline

let%expect_test _ = printReducedAst "[lam [] \"\" ]";
  [%expect{| Lam(params=[], value=Name("")) |}]

let%expect_test _ = printReducedAst "[[lam [] {}]]";
  [%expect{| Template() |}]

let%expect_test _ = printReducedAst "[[lam [(x)] {}] {} ]";
  [%expect{| Template() |}]

let%expect_test _ = print_endline (Ast.exprToString (Ast.ParsedAsm (Ast.exprToParsedAsm (expectAsm Environment.empty) (Ast.Asm " 12 "))))

let%expect_test _ = Ast.printAst {|
  [[lam [(x)] {
    [[lam [(x)] {
      addi zero zero x
    }] x]
    x
  }] { 12 }]
  |};
  [%expect {|  |}]


let%expect_test _ = printReducedAst {|
    [[lam [(x)] {
      addi t0 t1 x
    }] { 12 }]
  |};
  [%expect {|  |}]

let%expect_test _ = printReducedAst {|
  [[lam [(x)] {
    [[lam [(x)] {
      addi zero zero x
    }] x]
    slli t1 t2 x
  }] { 12 }]
  |};
  [%expect {|  |}]
