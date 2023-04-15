module E = Map.Make(String)

exception IllegalArgument of string * CharStream.range
exception WrongNumberOfArgs of string * CharStream.range
exception AssertionFail of string * CharStream.range

let assertNArgs pred description args r: unit =
  let len = List.length args in
  if pred len
    then raise (WrongNumberOfArgs ("Expected " ^ description ^ " arguments, but got " ^ (string_of_int len) ^ " arguments: " ^ (args |> List.map Ast.exprToString |> String.concat ", "), r))
    else ()
let assertExactlyNArgs n args r =
  assertNArgs ((<>) n) ("exactly " ^ (string_of_int n)) args r
let assertAtLeastNArgs n args r =
  assertNArgs ((>) n) ("at least " ^ (string_of_int n)) args r
let getNumericalArg r arg =
  (match arg with
  | Ast.Integer i, meta -> i, meta
  | _ -> raise (IllegalArgument ("Expected numerical arg, but got expression \"" ^ (Ast.exprToString arg) ^ "\"of the wrong type", r)))
let errorReportingPrintExpr e = SideEffects.unconditionalPrint e; print_endline (": " ^ (Ast.locationToString e))
let trueLambda = Ast.Lam {
  params=[]; lbody=[]; env=E.empty;
  f=fun args _ _ _ _ _ r -> assertExactlyNArgs 2 args r; List.hd args
}
let falseLambda = Ast.Lam {
  params=[]; lbody=[]; env=E.empty;
  f=fun args _ _ _ _ _ r -> assertExactlyNArgs 2 args r; List.nth args 1
}
let nargsAttr k = "nargs=" ^ (string_of_int k)
let varargsAttr = "varargs"
let hasNargsAttr k (metadata: Ast.metadata) =
  Ast.Attributes.mem (nargsAttr k) metadata.attrs || Ast.Attributes.mem varargsAttr metadata.attrs
let emptyLam nargs r closure f = Ast.Lam {params=[]; lbody=[]; env=closure; f=(
  fun args params lbody closure currentEnv evalSequence r ->
  assertExactlyNArgs nargs args r;
  f args params lbody closure currentEnv evalSequence r
)}, ({attrs=Ast.Attributes.singleton (nargsAttr nargs); r}: Ast.metadata)
let emptyVarargsLam r closure f = Ast.Lam {params=[]; lbody=[]; env=closure;f},
  ({attrs=Ast.Attributes.singleton (varargsAttr); r}: Ast.metadata)
let addattrInternal attr expr =
  let content, (meta': Ast.metadata) = expr in
    content, ({attrs = Ast.Attributes.add attr meta'.attrs; r = meta'.r}: Ast.metadata)
let print args _ _ closure _ _ r = (
  assertExactlyNArgs 1 args r;
  let arg = List.hd args in
  addattrInternal "print" (emptyLam 0 r closure (fun _ _ _ _ _ _ _ ->
  SideEffects.print arg))
)
let fail args _ _ closure _ _ r = (
  assertAtLeastNArgs 1 args r;
  emptyVarargsLam r closure (fun _ _ _ _ _  _ r  ->
  args |> List.iter errorReportingPrintExpr;
  raise (AssertionFail ("Assertion failed", r)))
)
let acceptFragment _ args r =
  assertExactlyNArgs 1 args r;
  let attr, meta = List.hd args in
  Ast.exprContentToString attr, meta
  (* match attr with
  | Ast.Asm s, meta -> s, meta
  | _ -> raise (AssertionFail (description ^ " " ^ (Ast.exprToString attr) ^ " is not a fragment", r)) *)
let addattr args _ _ closure _ _ r = (
  let s, _ = acceptFragment "Attribute" args r in
  emptyLam 1 r closure (fun args _ _ _ _ _ _ ->
    addattrInternal s (List.hd args)
  )
)
let hasattr args _ _ closure _ _ r = (
  let s, _ = acceptFragment "Attribute" args r in
  emptyLam 1 r closure (fun args _ _ _ _ _ _ ->
    let _, meta' = List.hd args in
    (if Ast.Attributes.mem s meta'.attrs then trueLambda else falseLambda), meta'
  )
)
let isLam args _ _ closure _ _ r = (
  assertExactlyNArgs 1 args r;
  let expectedNParams, meta = getNumericalArg r (List.hd args) in
  emptyLam 1 r closure (fun args _ _ _ _ _ _ -> (
    let arg = List.hd args in
    match arg with
    | Ast.Lam {params; _}, meta ->
      (if List.length params = expectedNParams || Ast.Attributes.mem (nargsAttr expectedNParams) meta.attrs || Ast.Attributes.mem varargsAttr meta.attrs then trueLambda else falseLambda), meta
    | _ -> falseLambda, meta
  ))
)
let lamOf paramChecks _ _ closure _ _ r = (
  List.iter (fun e -> match e with
    | Ast.Lam _, _ -> ()
    | _ -> raise (AssertionFail ("Expected lam but got " ^ (Ast.exprToString e), r))) paramChecks;
  emptyLam 1 r closure (fun args _ _ _ _ _ r ->
    let checkee = List.hd args in
    match checkee with
    | Lam (l: Ast.lam), meta -> (
      let nParams = List.length paramChecks in
      (let actualNParams = List.length l.params in
      (if actualNParams <> nParams && not (hasNargsAttr nParams (snd checkee)) then raise (AssertionFail ("Expected " ^ (string_of_int nParams) ^ " params, not " ^ (string_of_int actualNParams) ^ " ", (snd checkee).r))));
      let ({params; lbody; env; f}: Ast.lam) = l in
      let instrumented = Ast.Lam {params; lbody; env; f=fun args params lbody closure' currentEnv evalSequence r -> (
          (if List.length args <> nParams then raise (AssertionFail ("Wrong number of args: ", r)));
          let args' = List.map2
            (fun paramCheck arg -> Eval.evalExpr closure (Ast.LamApplication {lam=paramCheck; args=[arg]}, snd arg))
            paramChecks args |> List.map fst
          in f args' params lbody closure' currentEnv evalSequence r
        )}
      in instrumented, ({attrs=Ast.Attributes.add (nargsAttr nParams) meta.attrs; r=meta.r}: Ast.metadata))
    | _ -> raise (AssertionFail ("Expected lam but got " ^ (Ast.exprToString checkee), r))
  )
)
let isX predicate args _ _ _ _ _ r  = (
  assertExactlyNArgs 1 args r;
  let arg, meta' = List.hd args in
  if predicate arg then trueLambda, meta' else falseLambda, meta'
)
let isNum = isX (fun arg -> match arg with
  | Ast.Integer _ -> true
  | _ -> false)
let isFragment = isX (fun arg -> match arg with
  | Ast.ParsedAsm Assembly.Fragment _ -> true
  | Ast.Asm _ -> true
  | Ast.Template _ -> Ast.unwrap arg |> Option.is_some
  | _ -> false)
let isFinishedBlock = isX (fun arg -> match arg with
  | Ast.ParsedAsm Assembly.FinishedBlock _ -> true
  | Ast.ParsedAsm Assembly.Fragment s when String.trim s = "" -> true
  | _ -> false)
let isReg args _ _ _ currentEnv _ r  = (
  assertExactlyNArgs 1 args r;
  let arg, meta' = List.hd args in
  match Ast.unwrap arg with
  | Some s ->
    (try ignore(Assembly.nameToReg (Eval.expectAsm meta'.r currentEnv) (s, meta'.r)); trueLambda, meta'
    with _ -> falseLambda, meta')
  | _ -> falseLambda, meta')
let unsafeAssertKCyclesFb k (f: Assembly.finished_block): Assembly.finished_block = (
  {content=f.content; provides=f.provides; totalCycles = Some k; cyclesMod=f.cyclesMod})
let rec getCycles r (f: Assembly.finished_block) =
  let ({content; provides; totalCycles; cyclesMod}: Assembly.finished_block) = f in
  let totalCycles' = (match totalCycles with
  | Some k -> k
  | None -> (match f.content with
    | Assembly.Instruction i -> (match HardwareTable.cyclesOf i with
      | Some k -> k
      | None -> raise (AssertionFail ("Failed to determine exact number of cycles", r)))
    | Assembly.MetaBlock mb -> List.fold_left (+) 0 (List.map (getCycles r) mb |> List.map snd)))
  in ({content; provides; totalCycles=Some totalCycles'; cyclesMod}: Assembly.finished_block), totalCycles'
(* let getFinishedBlock _ r asmexpr =
  let failure = (IllegalArgument ("Expected argument of type FinishedBlock but got " ^ (Ast.exprToString asmexpr), r)) in
  match asmexpr with
  | Ast.ParsedAsm pasm, metadata -> (match pasm with
    | Assembly.FinishedBlock asmtem' -> asmtem', metadata
    (* | Assembly.Block b -> (match Assembly.promoteOrDemote env b with
      | Assembly.FinishedBlock asmtem' -> asmtem', metadata
      | _ -> raise failure) *)
    | _ -> raise failure)
  | _ -> raise failure *)
let rec parseFinishedBlockRec arg currentEnv r =
  match arg with
  | Ast.Template (tem, closure), metadata ->
    tem |> List.map (fun x -> parseFinishedBlockRec x closure r)
      |> List.map (fun (block, (metadata: Ast.metadata)) -> match block with
        | Ast.ParsedAsm (Assembly.FinishedBlock pasm) -> pasm
        | _ -> raise (IllegalArgument ("Expected parsed asm", metadata.r)))
      |> fun contents -> Assembly.MetaBlock contents
      |> Assembly.finishedBlockOf
      |> fun block -> Ast.ParsedAsm (Assembly.FinishedBlock block), metadata
  | Ast.Asm s, metadata ->
    s |> String.split_on_char '\n'
      |> List.filter (fun s -> String.trim s <> "")
      |> List.map (Assembly.tryParse (Eval.expectAsm r currentEnv))
      |> List.map (fun block -> (match block with
        | Some block -> Assembly.finishedBlockOf block
        | _ -> raise (IllegalArgument ("Expected valid assembly but got " ^ (Ast.exprToString arg), r))))
      |> fun it -> Assembly.MetaBlock it
      |> Assembly.finishedBlockOf
      |> fun block -> Ast.ParsedAsm (Assembly.FinishedBlock block), metadata
  | Ast.ParsedAsm pasm, metadata -> Ast.ParsedAsm pasm, metadata
  | _ -> raise (IllegalArgument ("Expected template or string but got " ^ (arg |> Ast.exprToString), r))
let parseFinishedBlock args _ _ _ currentEnv _ r = (
  assertExactlyNArgs 1 args r;
  parseFinishedBlockRec (List.hd args) currentEnv r)
let getFinishedBlock arg currentEnv r =
  let fb, metadata = parseFinishedBlockRec arg currentEnv r in
  match fb with
  | Ast.ParsedAsm (Assembly.FinishedBlock f) -> f, metadata
  | _ -> raise (IllegalArgument ("Expected ParsedAsm", r))
let exactCycles args _ _ _ currentEnv _ r =
  assertExactlyNArgs 1 args r;
  let asmexpr = args |> List.hd in
  let asmtem', metadata = getFinishedBlock asmexpr currentEnv r in
    let _, k = getCycles r asmtem' in
      Ast.ParsedAsm (Assembly.Fragment (string_of_int k)), metadata
let safeAssertKCycles args _ _ closure currentEnv _ r =
  assertExactlyNArgs 1 args r;
  let k, _ = args |> List.hd |> getNumericalArg r in
  emptyLam 1 r closure (fun args' _ _ _ _ _ _ ->
  args' |> List.hd |> fun x -> getFinishedBlock x currentEnv r
  |> fst |> getCycles r |> (
    fun (x, k') ->
      if k = k' then Ast.ParsedAsm (Assembly.FinishedBlock x)
      else raise (AssertionFail ("Expected " ^ (string_of_int k) ^ " cycles but got " ^ (string_of_int k'), r))),
  args |> List.hd |> snd)
let unsafeAssertKCycles args _ _ closure currentEnv _ r =
  assertExactlyNArgs 1 args r;
  emptyLam 1 r closure (fun args' _ _ _ _ _ _ ->
    let k, _ = args |> List.hd |> getNumericalArg r in
    let asmexpr = args' |> List.hd in
    let asmtem', metadata = getFinishedBlock asmexpr currentEnv r in
      Ast.ParsedAsm (Assembly.FinishedBlock (unsafeAssertKCyclesFb k asmtem')), metadata)
let binaryMathOp op args _ _ _ _ _ r =
  assertExactlyNArgs 2 args r;
  let arg0, _ = args |> List.hd |> getNumericalArg r in
  let arg1, _ = List.nth args 1 |> getNumericalArg r in
  Ast.Integer (op arg0 arg1), Ast.metaInitial r
let binaryMathOpMod op args _ _ closure _ _ r =
  assertExactlyNArgs 1 args r;
  let m, _ = getNumericalArg r (List.hd args) in
  emptyLam 2 r closure (binaryMathOp (fun a b -> let op'ed = (op a b) mod m in if op'ed >= 0 then op'ed else op'ed + m))
let assertNumericalComparisonResult comparator description args _ _ closure _ _ r =
  assertExactlyNArgs 1 args r;
  let arg0, _ = args |> List.hd |> getNumericalArg r in
  emptyLam 1 r closure (fun args _ _ _ _ _ _ ->
    let arg1expr = List.hd args in
    let arg1, r1 = arg1expr |> getNumericalArg r in
    (if comparator arg1 arg0 then arg1expr else raise (AssertionFail ("Expected " ^ description ^ " " ^ (string_of_int arg0) ^ " but got " ^ (string_of_int arg1), r1.r)))
  )
let foldRange args _ _ closure _ _ r =
  assertExactlyNArgs 3 args r;
  let args = fun x -> List.nth args x |> getNumericalArg r in
  let step, _ = args 0 in
  let start, _ = args 1 in
  let count, _ = args 2 in
  addattrInternal "range" (emptyLam 1 r closure (fun args _ _ closure _ _ r -> (
    let initial = List.hd args in
    emptyLam 1 r closure (fun args _ _ _ currentEnv _ r -> (
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
    ))
  )))
let applierifyVarargs args _ _ closure _ _ r =
  assertExactlyNArgs 1 args r;
  let applyee = List.hd args in
  emptyVarargsLam r closure (fun varargs _ _ closure' currentEnv _ r' -> (
    Eval.evalExpr currentEnv (Ast.LamApplication
    {lam=applyee; args=[emptyLam 1 r' closure' (fun args _ _ _ _ _ r'' -> (
      let nestedApplyee = List.hd args in
      Eval.evalExpr currentEnv (
        Ast.LamApplication {lam=nestedApplyee; args=varargs}, Ast.metaInitial r''
      ) |> fst
    ))]}, Ast.metaInitial r') |> fst)
  )
let stdFun: Ast.lam_function E.t = E.empty
  |> E.add "lam" Eval.lam
  |> E.add "mu" Eval.mu
  |> E.add "print" print
  |> E.add "fail" fail
  |> E.add "cycles?" exactCycles
  |> E.add "cycles!" safeAssertKCycles
  |> E.add "unsafe-assert-exact-cycles" unsafeAssertKCycles
  |> E.add "block!" parseFinishedBlock
  |> E.add "addattr" addattr
  |> E.add "hasattr" hasattr
  |> E.add "lam?" isLam
  |> E.add "lamof" lamOf
  |> E.add "num?" isNum
  |> E.add "frag?" isFragment
  |> E.add "block?" isFinishedBlock
  |> E.add "reg?" isReg
  |> E.add "+" (binaryMathOp (+))
  |> E.add "*" (binaryMathOp ( * ))
  |> E.add "-" (binaryMathOp (-))
  |> E.add "/" (binaryMathOp (/))
  |> E.add "%" (binaryMathOp (mod))
  |> E.add "mod+" (binaryMathOpMod (+))
  |> E.add "mod*" (binaryMathOpMod ( * ))
  |> E.add "mod-" (binaryMathOpMod (-))
  |> E.add "mod/" (binaryMathOpMod (/))
  |> E.add "=!" (assertNumericalComparisonResult (=) "exactly")
  |> E.add "<!" (assertNumericalComparisonResult (<) "less than")
  |> E.add "<=!" (assertNumericalComparisonResult (<=) "less than or equal to")
  |> E.add ">!" (assertNumericalComparisonResult (>) "greater than")
  |> E.add ">=!" (assertNumericalComparisonResult (>=) "greater than or equal to")
  |> E.add "fold-range" foldRange
  |> E.add "applierify-varargs" applierifyVarargs

let std = E.map (fun f -> addattrInternal "std" (Ast.Lam {params=[]; lbody=[]; env=E.empty; f}, Ast.metaEmpty)) stdFun

let%expect_test _ = (try
  Eval.printReducedAst stdFun std {|
  [[
    [lam [(true) (false)] true]
    {help}
    [lam [] [fail {help}]]]]
  |} with
    | AssertionFail (s, r) -> print_endline s; print_endline (CharStream.rangeToString r)
    | Eval.EvalFail (s, _) -> print_string s);
  [%expect{| Expected Lam but got E(Template(E(Asm(help), )), ) |}]

let%expect_test _ = (try
  Eval.printReducedAst stdFun std {|
  [[
    [lam [(true) (false)] [false]]
    [lam [] {help}]
    [lam [] [fail {help}]]]]
  |} with
    | AssertionFail (s, r) -> print_endline s; print_endline (CharStream.rangeToString r)
    | Eval.EvalFail (s, _) -> print_endline s);
  [%expect{|
    help: [] line 5, col 19 to line 5, col 25
    Assertion failed
    [] line 2, col 4 to line 5, col 29 |}]

let%expect_test _ = (try Eval.printReducedAst stdFun std {|
    [cycles?
      {
        lw t0 0(a1)
        addi t0 t0 12
      }]
  |} with
    | AssertionFail (s, r) -> print_endline s; print_endline (CharStream.rangeToString r));
  [%expect{| E(ParsedAsm(Fragment(3)), ) |}]
