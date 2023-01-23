module E = Map.Make(String)

exception IllegalArgument of string
exception WrongNumberOfArgs of string
exception AssertionFail of string

let assertNArgs pred description args: unit =
  let len = List.length args in
  if pred len
    then raise (WrongNumberOfArgs ("Expected " ^ description ^ " arguments, but got " ^ (string_of_int len) ^ " arguments: " ^ (args |> List.map Ast.exprToString |> String.concat ", ") ^ (match List.nth_opt args 0 with | Some a -> Ast.locationToString a | _ -> "")))
    else ()
let assertExactlyNArgs n args =
  assertNArgs ((<>) n) ("exactly " ^ (string_of_int n)) args
let assertAtLeastNArgs n args =
  assertNArgs ((>) n) ("at least " ^ (string_of_int n)) args
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
  f=fun args _ _ _ _ _ _ -> assertExactlyNArgs 2 args; List.hd args
}
let falseLambda = Ast.Lam {
  params=[]; lbody=[]; env=E.empty;
  f=fun args _ _ _ _ _ _ -> assertExactlyNArgs 2 args; List.nth args 1
}
let emptyLam closure f = Ast.Lam {params=[]; lbody=[]; env=closure; f}
let print args _ _ closure _ _ _ = (
  assertExactlyNArgs 1 args;
  let arg = List.hd args in
  emptyLam closure (fun _ _ _ _ _ _ _ ->
  let sideEffectsAllowed = Array.get Sys.argv 1 <> "tokens" in
  (match arg with
  | Ast.ParsedAsm pasm, _ -> if sideEffectsAllowed then Assembly.print pasm; arg
  | _ -> if sideEffectsAllowed then print_endline (Ast.exprToString arg); arg)),
  snd arg
)
let fail args _ _ closure _ _ _ = (
  assertAtLeastNArgs 1 args;
  emptyLam closure (fun _ _ _ _ _ _  ->
  args |> List.tl |> List.iter errorReportingPrintExpr;
  raise (AssertionFail ("Assertion failed: " ^ (Ast.locationToString (List.hd args))))),
  args |> List.hd |> snd
)
let acceptFragment description args =
  assertExactlyNArgs 1 args;
  let attr = List.hd args in
  match attr with
  | Ast.ParsedAsm Assembly.Fragment s, meta ->
    s, meta
  | _ -> raise (AssertionFail (description ^ " is not a fragment" ^ (Ast.locationToString attr)))
let addattr args _ _ closure _ _ _ = (
  let s, meta = acceptFragment "Attribute" args in
  emptyLam closure (fun args _ _ _ _ _ _ ->
    assertExactlyNArgs 1 args;
    let content, meta' = List.hd args in
    content, ({attrs = Ast.Attributes.add s meta'.attrs; r = meta'.r}: Ast.metadata)
  ), meta
)
let hasattr args _ _ closure _ _ _ = (
  let s, meta = acceptFragment "Attribute" args in
  emptyLam closure (fun args _ _ _ _ _ _ ->
    assertExactlyNArgs 1 args;
    let _, meta' = List.hd args in
    (if Ast.Attributes.mem s meta'.attrs then trueLambda else falseLambda), meta'
  ), meta
)
let isLam args _ _ closure _ _ _ = (
  assertExactlyNArgs 1 args;
  let expectedNParams, meta = getNumericalArg (List.hd args) in
  emptyLam closure (fun args _ _ _ _ _ _ -> (
    assertExactlyNArgs 1 args;
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
  emptyLam closure (fun args _ _ _ _ _ _ ->
    assertExactlyNArgs 1 args;
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
let isX predicate args _ _ _ _ _ _  = (
  assertExactlyNArgs 1 args;
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
let isReg args _ _ _ currentEnv _ _  = (
  assertExactlyNArgs 1 args;
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
let exactCycles args _ _ _ _ _ _ =
  assertExactlyNArgs 2 args;
  let kexpr = List.hd args in
  let asmexpr = args |> List.tl |> List.hd in
  match kexpr, asmexpr with
  | (Ast.ParsedAsm (Assembly.Fragment ktem'), _),
    (Ast.ParsedAsm (Assembly.FinishedBlock asmtem'), metadata) -> (
    let k = int_of_string ktem' in
    match getCycles asmtem' with
    | Some k' -> if k <> k'
      then raise (AssertionFail ("Expected assembly taking " ^ (string_of_int k) ^ " cycles but got assembly taking " ^ (string_of_int k') ^ " cycles."))
      else Ast.ParsedAsm (
        Assembly.FinishedBlock (unsafeAssertKCyclesFb k asmtem')),
        metadata
    | None -> raise (AssertionFail "Failed to determine exact number of cycles"))
  | _ -> raise (IllegalArgument ("Expected two arguments of type Fragment and FinishedBlock respectively, but got " ^ (Ast.exprToString kexpr) ^ " and " ^ (Ast.exprToString asmexpr)))

let std: Ast.lam_function E.t = E.empty
  |> E.add "lam" Eval.lam
  |> E.add "mu" Eval.mu
  |> E.add "print" print
  |> E.add "fail" fail
  |> E.add "exact_cycles" exactCycles
  |> E.add "addattr" addattr
  |> E.add "hasattr" hasattr
  |> E.add "lam?" isLam
  |> E.add "lamof" lamOf
  |> E.add "num?" isNum
  |> E.add "frag?" isFragment
  |> E.add "block?" isFinishedBlock
  |> E.add "reg?" isReg

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
  [%expect{| Assertion failed: line 4, col 18 to line 4, col 24 |}]

let%expect_test _ = (try Eval.printReducedAst std {|
    [exact_cycles {4}
      {
        lw t0 0(a1)
        addi t0 t0 12
      }]
  |} with
    | AssertionFail s -> print_string s);
  [%expect{| Expected assembly taking 4 cycles but got assembly taking 3 cycles. |}]
