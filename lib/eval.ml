module Environment = Map.Make(String)

exception EvalFail of string * CharStream.range

let rec evalExpr env e = match e with
  | Ast.Name str, _ -> if Environment.mem str env
    then evalExpr env (Environment.find str env)
    else raise (EvalFail ("Unbound name: " ^ str, Ast.rangeOf e))
  | Ast.Var _, _ -> raise (EvalFail (
    "A parameter is not an expression, but tried to evaluate "
    ^ (Ast.exprToString e), Ast.rangeOf e))
  | Ast.ParsedAsm _, _ -> e, env
  | Ast.Asm asm, meta -> (Ast.ParsedAsm
      (Assembly.parse Assembly.empty (expectAsm env) asm),
      meta
    ), env
  | Ast.Template tem, meta ->
    let exprs', env' = evalExprListInOrder env tem in
    let tem = (Ast.Template exprs', meta) in
    let pasm, meta = Ast.exprToParsedAsm (expectAsm env') tem in
    (ParsedAsm pasm, meta), env'
  | Ast.Lam {params; lbody; env = prevEnv; f }, meta -> (
    if Environment.is_empty prevEnv
      then (Ast.Lam { params; lbody; env; f }, meta), env
      else e, env)
  | Ast.LamApplication { lam; args }, _ -> (
    let args', env' = evalExprListInOrder env args in (
    let lam', _ = evalExpr env' lam in
    match lam' with
    | Lam { params; lbody; env = env''; f }, _ ->
      f args' params lbody env'' env' evalSequence (Ast.rangeOf e), env'
    | e -> raise (EvalFail ("Expected Lam but got " ^ Ast.exprToString e, Ast.rangeOf e))))
  | Ast.Def define, _ -> (let evaluated = evalSequence env define.dvalue in
    evaluated, bindNames env [fst define.dname] [evaluated] (Ast.rangeOf e))
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
  | (Ast.Asm num, meta), _ -> num, meta.r
  | (Ast.ParsedAsm pasm, meta), _ -> (match pasm with
    | Fragment f -> f, meta.r
    | _ -> raise (Assembly.AsmParseFail ("Expected assembly fragment, but got assembly block")))
  | bad, _ -> raise (Assembly.AsmParseFail (
    description ^ " expected, but found expression " ^ Ast.exprToString bad))
  else Assembly.failWithNoBinding name)
and bindNames env params args r =
  let nparams = List.length params in
  let nargs = List.length args in
  if nparams = nargs
    then List.fold_left2
      (fun env param arg ->
        let p : Ast.var = param in
        let arg', env' = applyChecks env p arg in
          Environment.add p.name arg' env')
      env params args
    else raise (EvalFail ("Expected " ^ (string_of_int nparams) ^ " arguments corresponding to parameters " ^ (String.concat ", " (List.map (fun (v: Ast.var) -> v.name) params)) ^ " but got " ^ (string_of_int nargs) ^ " arguments: " ^ (String.concat ", " (List.map Ast.exprToString args)), r))
and applyChecks env (param: Ast.var) arg =
  List.fold_left (fun (arg, env') (check, meta) ->
    evalExpr env' (LamApplication {lam=check; args=[arg]}, meta)
  ) (arg, env) param.checks

let lam args params lbody closure _ evalSequence r =
  let bound = bindNames closure (List.map fst params) args r in
    evalSequence bound lbody
let mu args params lbody _ currentEnv evalSequence r =
  let bound = bindNames currentEnv (List.map fst params) args r in
    evalSequence bound lbody
let testStd = Environment.empty
  |> Environment.add "lam" lam
  |> Environment.add "mu" mu


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
    Block(, MetaBlock(RType(add, temp-t0, temp-t1, temp-t2)
    IArith(addi, temp-a0, temp-a1, 0x66)
    Load(lbu, save-s4, temp-t6, 12)
    Branch(beq, temp-a5, temp-t3, END)
    Jal(jal, Ra, END3)
    Jalr(jalr, Zero, temp-t4, END2)),   ) |}]

let printReducedAst std text =
  text |> Ast.getAst std |> evalExpr Environment.empty |> fst |> Ast.exprToString
  |> print_endline
let printEndingAsm text =
  text |> Ast.getAst testStd |> evalExpr Environment.empty |> fst |> fun x -> match x with | ParsedAsm pasm, _ -> Assembly.print pasm | _ -> print_endline "did not evaluate to ParsedAsm"

let%expect_test _ = printReducedAst testStd "[lam [] \"\" ]";
  [%expect{| Lam(params=[], lbody=Name("")) |}]

let%expect_test _ = printReducedAst testStd "[[lam [] {}]]";
  [%expect{| ParsedAsm(Fragment()) |}]

let%expect_test _ = printReducedAst testStd "[[lam [(x)] {}] {} ]";
  [%expect{| ParsedAsm(Fragment()) |}]

let%expect_test _ = print_endline (Ast.exprToString (Ast.ParsedAsm (fst (Ast.exprToParsedAsm (expectAsm Environment.empty) (Ast.Asm " 12 ", Ast.metaEmpty))), Ast.metaEmpty));
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


let%expect_test _ = printReducedAst testStd {|
    [[lam [(x)] {
      addi t0 t1 x
    }] { 12 }]
  |};
  [%expect {| ParsedAsm(MetaBlock(IArith(addi, temp-t0, temp-t1,  12 ))) |}]

let%expect_test _ = printReducedAst testStd {|
  [[lam [(x)] {
    [[lam [(x)] {
      addi zero zero x
    }] x]
    slli t1 t2 x
  }] { 12 }]
  |};
  [%expect {|
    ParsedAsm(Block(
        , MetaBlock(MetaBlock(IArith(addi, Zero, Zero,  12 ))
    IArith(slli, temp-t1, temp-t2,  12 )), )) |}]

let%expect_test _ = printReducedAst testStd {|
    [[lam [(x)]
      [def (a) x]
      {add t0 t1 t2}
      a
    ] {addi zero zero 0} ]
  |};
  [%expect{| ParsedAsm(MetaBlock(IArith(addi, Zero, Zero, 0))) |}]

let%expect_test _ = printReducedAst testStd {|
    [
      [
        [lam [(x)]
          [def (bound_in_closure) {t0}]
          [lam [(y)] {sub bound_in_closure x y}]]
        {t1}]
      {t2}]
  |};
  [%expect{| ParsedAsm(MetaBlock(RType(sub, temp-t0, temp-t1, temp-t2))) |}]

let%expect_test _ = printReducedAst testStd {|
    [[lam []
      [def (x) {t0}]
      [def (m) [[lam [(x)] [mu [] {lw x 0(a0)}]] {a0}]]
      [m]]]
  |};
  [%expect{| ParsedAsm(MetaBlock(Load(lw, temp-t0, temp-a0, 0))) |}]


let%expect_test _ = printReducedAst testStd {|
  [[lam []
    [def (x) {t0}]
    [def (m) [
      [lam [(z)]
        [lam [] {lw z 0(a0)}]]
      {a0}]]
    [m]]]
  |};
  [%expect{| ParsedAsm(MetaBlock(Load(lw, temp-a0, temp-a0, 0))) |}]

let%expect_test _ = printReducedAst testStd {|
  {
    START:
    jal ra START
  }
  |};
  [%expect{|
    ParsedAsm(MetaBlock(Label(START:)
    Jal(jal, Ra, START))) |}]

let%expect_test _ = printEndingAsm {|
  {
    START:
    [[lam [(x)] x] {
      START:
      bne t0 t1 START
    }]
    jal gp START
  }
  |};
  [%expect {|
    START_5TsUpEQjDf:
    START_Kd4gRhlUDG:
        bne t0, t1 START_Kd4gRhlUDG
        jal gp START_5TsUpEQjDf |}]

let%expect_test _ = printReducedAst testStd {|
  [[lam [(x [lam [(la)] [la]])] x]
    [lam [] {hello}]]
  |};
  [%expect{| ParsedAsm(Fragment(hello)) |}]

let%expect_test _ = printReducedAst testStd {|
  {
    lui t1 0x40000
    auipc a0 0x12345
    li t1 0x80000001
    mv t0 t1
    neg t0 t1
    nop
    not t0 t1
    ret
    csrw 0x51e s6
    csrrw t0 sp 0x51e
    rdcycle s0
  }
|};
  [%expect {|
    ParsedAsm(MetaBlock(UType(lui, temp-t1, 0x40000)
    UType(auipc, temp-a0, 0x12345)
    MetaBlock(UType(lui, temp-t1, 524288)
    IArith(addi, temp-t1, Zero, 1))
    IArith(addi, temp-t0, temp-t1, 0)
    RType(sub, temp-t0, temp-t1, Zero)
    IArith(addi, Zero, Zero, 0)
    IArith(xori, temp-t0, temp-t1, -1)
    Jalr(jalr, Zero, Ra, 0)
    IArith(csrrw, Zero, save-s6, 0x51e)
    IArith(csrrw, temp-t0, Sp, 0x51e)
    IArith(csrrs, save-s0, Zero, 0xb00))) |}]
