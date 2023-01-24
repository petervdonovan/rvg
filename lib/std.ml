module E = Map.Make(String)

exception IllegalArgument of string
exception WrongNumberOfArgs of string
exception AssertionFail of string

let assertNArgs pred description args r: unit =
  let len = List.length args in
  if pred len
    then raise (WrongNumberOfArgs ("Expected " ^ description ^ " arguments, but got " ^ (string_of_int len) ^ " arguments: " ^ (args |> List.map Ast.exprToString |> String.concat ", ") ^ (CharStream.rangeToString r)))
    else ()
let assertExactlyNArgs n args r =
  assertNArgs ((<>) n) ("exactly " ^ (string_of_int n)) args r
let assertAtLeastNArgs n args r =
  assertNArgs ((>) n) ("at least " ^ (string_of_int n)) args r
let getStringArgOpt arg =
  match arg with
  | Ast.ParsedAsm (Assembly.Fragment n), meta -> Some (n, meta)
  | _ -> None
let getNumericalArg arg =
  match getStringArgOpt arg with
  | Some (n, meta) -> (match int_of_string_opt n with
    | None -> raise (IllegalArgument ("\"" ^ n ^ "\" is not a number"))
    | Some k -> k, meta)
  | _ -> raise (IllegalArgument ("Expected numerical arg, but got expression of the wrong type: " ^ (Ast.locationToString arg)))
let errorReportingPrintExpr e =
  print_endline ((match getStringArgOpt e with
  | Some (s, _) -> s
  | None -> Ast.exprToString e)  ^ ": " ^ (Ast.locationToString e))
let trueLambda = Ast.Lam {
  params=[]; lbody=[]; env=E.empty;
  f=fun args _ _ _ _ _ r -> assertExactlyNArgs 2 args r; List.hd args
}
let falseLambda = Ast.Lam {
  params=[]; lbody=[]; env=E.empty;
  f=fun args _ _ _ _ _ r -> assertExactlyNArgs 2 args r; List.nth args 1
}
let emptyLam closure f = Ast.Lam {params=[]; lbody=[]; env=closure; f}
let print args _ _ closure _ _ r = (
  assertExactlyNArgs 1 args r;
  let arg = List.hd args in
  emptyLam closure (fun _ _ _ _ _ _ _ ->
  let sideEffectsAllowed = Array.get Sys.argv 1 <> "tokens" in
  (match arg with
  | Ast.ParsedAsm pasm, _ -> if sideEffectsAllowed then Assembly.print pasm; arg
  | _ -> if sideEffectsAllowed then print_endline (Ast.exprToString arg); arg)),
  snd arg
)
let fail args _ _ closure _ _ r = (
  assertAtLeastNArgs 1 args r;
  emptyLam closure (fun _ _ _ _ _  _ r  ->
  args |> List.tl |> List.iter errorReportingPrintExpr;
  raise (AssertionFail ("Assertion failed: " ^ (CharStream.rangeToString r)))),
  args |> List.hd |> snd
)
let acceptFragment description args r =
  assertExactlyNArgs 1 args r;
  let attr = List.hd args in
  match attr with
  | Ast.ParsedAsm Assembly.Fragment s, meta ->
    s, meta
  | _ -> raise (AssertionFail (description ^ " is not a fragment" ^ (Ast.locationToString attr)))
let addattr args _ _ closure _ _ r = (
  let s, meta = acceptFragment "Attribute" args r in
  emptyLam closure (fun args _ _ _ _ _ r ->
    assertExactlyNArgs 1 args r;
    let content, meta' = List.hd args in
    content, ({attrs = Ast.Attributes.add s meta'.attrs; r = meta'.r}: Ast.metadata)
  ), meta
)
let hasattr args _ _ closure _ _ r = (
  let s, meta = acceptFragment "Attribute" args r in
  emptyLam closure (fun args _ _ _ _ _ r ->
    assertExactlyNArgs 1 args r;
    let _, meta' = List.hd args in
    (if Ast.Attributes.mem s meta'.attrs then trueLambda else falseLambda), meta'
  ), meta
)
let isLam args _ _ closure _ _ r = (
  assertExactlyNArgs 1 args r;
  let expectedNParams, meta = getNumericalArg (List.hd args) in
  emptyLam closure (fun args _ _ _ _ _ r -> (
    assertExactlyNArgs 1 args r;
    let arg = List.hd args in
    match arg with
    | Ast.Lam {params; _}, _ ->
      (if List.length params = expectedNParams then trueLambda else falseLambda), meta
    | _ -> falseLambda, meta
  )), meta
)
let lamOf paramChecks _ _ closure _ _ _ = (
  List.iter (fun e -> match e with
    | Ast.Lam _, _ -> ()
    | _ -> raise (AssertionFail ("Expected lam " ^ Ast.locationToString e))) paramChecks;
  emptyLam closure (fun args _ _ _ _ _ r ->
    assertExactlyNArgs 1 args r;
    let checkee = List.hd args in
    match checkee with
    | Lam (l: Ast.lam), meta -> (
      let nParams = List.length paramChecks in
      (let actualNParams = List.length l.params in
      (if actualNParams <> nParams then raise (AssertionFail ("Expected " ^ (string_of_int nParams) ^ " params, not " ^ (string_of_int actualNParams) ^ " " ^ Ast.locationToString checkee))));
      let ({params; lbody; env; f}: Ast.lam) = l in
      let instrumented = Ast.Lam {params; lbody; env; f=fun args params lbody closure' currentEnv evalSequence r -> (
          (if List.length args <> nParams then raise (AssertionFail ("Wrong number of args: " ^ CharStream.rangeToString r)));
          let args' = List.map2
            (fun paramCheck arg -> Eval.evalExpr closure (Ast.LamApplication {lam=paramCheck; args=[arg]}, snd arg))
            paramChecks args |> List.map fst
          in f args' params lbody closure' currentEnv evalSequence r
        )}
      in instrumented, meta)
    | _ -> raise (AssertionFail ("Expected lam " ^ Ast.locationToString checkee))
  ), Ast.metaEmpty
)
let isX predicate args _ _ _ _ _ r  = (
  assertExactlyNArgs 1 args r;
  let arg, meta' = List.hd args in
  if predicate arg then trueLambda, meta' else falseLambda, meta'
)
let isNum = isX (fun arg -> match arg with
  | Ast.ParsedAsm Assembly.Fragment s ->
    (match int_of_string_opt s with
    | Some _ -> true
    | None -> false)
  | _ -> false)
let isFragment = isX (fun arg -> match arg with
  | Ast.ParsedAsm Assembly.Fragment _ -> true
  | _ -> false)
let isFinishedBlock = isX (fun arg -> match arg with
  | Ast.ParsedAsm Assembly.FinishedBlock _ -> true
  | _ -> false)
let isReg args _ _ _ currentEnv _ r  = (
  assertExactlyNArgs 1 args r;
  let arg, meta' = List.hd args in
  match arg with
  | Ast.ParsedAsm Assembly.Fragment s ->
    (try ignore(Assembly.nameToReg (Eval.expectAsm currentEnv) (s, meta'.r)); trueLambda, meta'
    with _ -> falseLambda, meta')
  | _ -> falseLambda, meta')
let unsafeAssertKCyclesFb k (f: Assembly.finished_block): Assembly.finished_block = (
  {content=f.content; provides=f.provides; totalCycles = Some k; cyclesMod=f.cyclesMod})
let rec getCycles (f: Assembly.finished_block) = match f.totalCycles with
  | Some k -> Some k
  | None -> (match f.content with
    | Assembly.Instruction i -> HardwareTable.cyclesOf i
    | Assembly.MetaBlock mb -> List.fold_left
      (fun a b -> match a, b with | Some k, Some n -> Some (k + n) | _ -> None)
      (Some 0)
      (List.map getCycles mb))
let getFinishedBlock env asmexpr =
  let failure = (IllegalArgument ("Expected argument of type FinishedBlock but got " ^ (Ast.exprToString asmexpr))) in
  match asmexpr with
  | Ast.ParsedAsm pasm, metadata -> (match pasm with
    | Assembly.FinishedBlock asmtem' -> asmtem', metadata
    | Assembly.Block b -> (match Assembly.promoteOrDemote env b with
      | Assembly.FinishedBlock asmtem' -> asmtem', metadata
      | _ -> raise failure)
    | _ -> raise failure)
  | _ -> raise failure
let exactCycles args _ _ _ currentEnv _ r =
  assertExactlyNArgs 1 args r;
  let asmexpr = args |> List.hd in
  let asmtem', metadata = getFinishedBlock (Eval.expectAsm currentEnv) asmexpr in
    match getCycles asmtem' with
    | Some k -> Ast.ParsedAsm (
        Assembly.Fragment (string_of_int k)),
        metadata
    | None -> raise (AssertionFail "Failed to determine exact number of cycles")
let unsafeAssertKCycles args _ _ closure currentEnv _ r =
  assertExactlyNArgs 1 args r;
  emptyLam closure (fun args' _ _ _ _ _ r ->
    assertExactlyNArgs 1 args' r;
    let k, _ = args |> List.hd |> getNumericalArg in
    let asmexpr = args' |> List.hd in
    let asmtem', metadata = getFinishedBlock (Eval.expectAsm currentEnv) asmexpr in
      Ast.ParsedAsm (Assembly.FinishedBlock (unsafeAssertKCyclesFb k asmtem')), metadata),
      Ast.metaInitial r
let binaryMathOp op args _ _ _ _ _ r =
  assertExactlyNArgs 2 args r;
  let arg0, _ = args |> List.hd |> getNumericalArg in
  let arg1, _ = List.nth args 1 |> getNumericalArg in
  Ast.ParsedAsm (Assembly.Fragment (string_of_int (op arg0 arg1))), Ast.metaInitial r
let plus = binaryMathOp (+)
let minus = binaryMathOp (-)
let times = binaryMathOp ( * )
let dividedBy = binaryMathOp (/)
let assertNumericalComparisonResult comparator description args _ _ closure _ _ r =
  assertExactlyNArgs 1 args r;
  let arg0, _ = args |> List.hd |> getNumericalArg in
  emptyLam closure (fun args _ _ _ _ _ r ->
    assertExactlyNArgs 1 args r;
    let arg1expr = List.hd args in
    let arg1, r1 = arg1expr |> getNumericalArg in
    (if comparator arg1 arg0 then arg1expr else raise (AssertionFail ("Expected " ^ description ^ " " ^ (string_of_int arg0) ^ " but got " ^ (string_of_int arg1) ^ ": " ^ Ast.locationToString (arg1, r1))))
  ), Ast.metaInitial r
let foldRange args _ _ closure _ _ r =
  assertExactlyNArgs 3 args r;
  let args = fun x -> List.nth args x |> getNumericalArg in
  let step, _ = args 0 in
  let start, _ = args 1 in
  let count, _ = args 2 in
  emptyLam closure (fun args _ _ closure _ _ r -> (
    assertExactlyNArgs 1 args r;
    let initial = List.hd args in
    emptyLam closure (fun args _ _ _ currentEnv _ r -> (
      assertExactlyNArgs 1 args r;
      Seq.fold_left
        (fun acc next -> fst (Eval.evalExpr currentEnv (
          Ast.LamApplication {lam=List.hd args; args=[acc;next]}, Ast.metaInitial r
        )))
        initial
        ((Seq.unfold (fun (current, n) ->
          if n = count then None
          else Some (current, (current + step, n + 1))) (start, 0))
          |> Seq.map (fun x ->
            Ast.ParsedAsm (Assembly.Fragment (string_of_int x)), Ast.metaInitial r))
    )), Ast.metaInitial r
  )), Ast.metaInitial r
let applierifyVarargs args _ _ closure _ _ r =
  assertExactlyNArgs 1 args r;
  let applyee = List.hd args in
  emptyLam closure (fun varargs _ _ closure' currentEnv _ r' -> (
    Eval.evalExpr currentEnv (Ast.LamApplication
    {lam=applyee; args=[emptyLam closure' (fun args _ _ _ _ _ r'' -> (
      assertExactlyNArgs 1 args r;
      let nestedApplyee = List.hd args in
      Eval.evalExpr currentEnv (
        Ast.LamApplication {lam=nestedApplyee; args=varargs}, Ast.metaInitial r''
      ) |> fst
    )), Ast.metaInitial r']}, Ast.metaInitial r') |> fst)
  ), Ast.metaInitial r
let std: Ast.lam_function E.t = E.empty
  |> E.add "lam" Eval.lam
  |> E.add "mu" Eval.mu
  |> E.add "print" print
  |> E.add "fail" fail
  |> E.add "cycles?" exactCycles
  |> E.add "unsafe-assert-exact-cycles" unsafeAssertKCycles
  |> E.add "addattr" addattr
  |> E.add "hasattr" hasattr
  |> E.add "lam?" isLam
  |> E.add "lamof" lamOf
  |> E.add "num?" isNum
  |> E.add "frag?" isFragment
  |> E.add "block?" isFinishedBlock
  |> E.add "reg?" isReg
  |> E.add "+" plus
  |> E.add "*" times
  |> E.add "-" minus
  |> E.add "/" dividedBy
  |> E.add "=!" (assertNumericalComparisonResult (=) "exactly")
  |> E.add "<!" (assertNumericalComparisonResult (<) "less than")
  |> E.add "<=!" (assertNumericalComparisonResult (<=) "less than or equal to")
  |> E.add ">!" (assertNumericalComparisonResult (>) "greater than")
  |> E.add ">=!" (assertNumericalComparisonResult (>=) "greater than or equal to")
  |> E.add "fold_range" foldRange
  |> E.add "applierify-varargs" applierifyVarargs

let%expect_test _ = (try
  Eval.printReducedAst std {|
  [[
    [lam [(true) (false)] true]
    {help}
    [lam [] [fail {help}]]]]
  |} with
    | AssertionFail s -> print_string s
    | Eval.EvalFail (s, _) -> print_string s);
  [%expect{| Expected Lam but got ParsedAsm(Fragment(help)) |}]

let%expect_test _ = (try
  Eval.printReducedAst std {|
  [[
    [lam [(true) (false)] [false]]
    [lam [] {help}]
    [lam [] [fail {help}]]]]
  |} with
    | AssertionFail s -> print_string s
    | Eval.EvalFail (s, _) -> print_string s);
  [%expect{| Assertion failed: line 1, col 3 to line 4, col 28 |}]

let%expect_test _ = (try Eval.printReducedAst std {|
    [cycles?
      {
        lw t0 0(a1)
        addi t0 t0 12
      }]
  |} with
    | AssertionFail s -> print_string s);
  [%expect{| ParsedAsm(Fragment(3)) |}]
