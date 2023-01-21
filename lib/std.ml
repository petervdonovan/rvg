module E = Map.Make(String)

exception IllegalArgument of string
exception WrongNumberOfArgs of string
exception AssertionFail of string

let assertNArgs pred description args: unit =
  let len = List.length args in
  if pred len
    then raise (WrongNumberOfArgs ("Expected " ^ description ^ " arguments, but got " ^ (string_of_int len) ^ " arguments."))
    else ()
let assertExactlyNArgs n args =
  assertNArgs ((<>) n) ("exactly " ^ (string_of_int n)) args
let assertAtLeastNArgs n args =
  assertNArgs ((>) n) ("at least " ^ (string_of_int n)) args

let print args _ _ _ _ _ = (
  assertExactlyNArgs 1 args;
  let arg = List.hd args in
  (match arg with
  | Ast.ParsedAsm (pasm, _) -> Assembly.print pasm; arg
  | _ -> raise (IllegalArgument ("Expected ParsedAsm, but got " ^ Ast.exprToString arg)))
)
let fail args _ _ _ _ _ = (
  assertAtLeastNArgs 1 args;
  args |> List.tl |> List.iter (fun e -> print_endline (Ast.exprToString e));
  raise (AssertionFail ("Assertion failed: " ^ (Ast.locationToString (List.hd args))))
)

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
let exactCycles args _ _ _ _ _ =
  assertExactlyNArgs 2 args;
  let kexpr = List.hd args in
  let asmexpr = args |> List.tl |> List.hd in
  match kexpr, asmexpr with
  | Ast.ParsedAsm (Assembly.Fragment ktem', _),
    Ast.ParsedAsm (Assembly.FinishedBlock asmtem', metadata) -> (
    let k = int_of_string ktem' in
    match getCycles asmtem' with
    | Some k' -> if k <> k'
      then raise (AssertionFail ("Expected assembly taking " ^ (string_of_int k) ^ " cycles but got assembly taking " ^ (string_of_int k') ^ " cycles."))
      else Ast.ParsedAsm (
        Assembly.FinishedBlock (unsafeAssertKCyclesFb k asmtem'),
        metadata
      )
    | None -> raise (AssertionFail "Failed to determine exact number of cycles"))
  | _ -> raise (IllegalArgument ("Expected two arguments of type Fragment and FinishedBlock respectively, but got " ^ (Ast.exprToString kexpr) ^ " and " ^ (Ast.exprToString asmexpr)))

let std: Ast.lam_function E.t = E.empty
  |> E.add "lam" Eval.lam
  |> E.add "mu" Eval.mu
  |> E.add "print" print
  |> E.add "fail" fail
  |> E.add "exact_cycles" exactCycles

let%expect_test _ = (try
  Eval.printReducedAst std {|
  [[
    [lam [(true) (false)] true]
    {help}
    [lam [] [fail {help}]]]]
  |} with
    | AssertionFail s -> print_string s
    | Eval.EvalFail s -> print_string s);
  [%expect{| Expected Lam but got ParsedAsm(Fragment(help)) |}]

let%expect_test _ = (try
  Eval.printReducedAst std {|
  [[
    [lam [(true) (false)] false]
    [lam [] {help}]
    [lam [] [fail {help}]]]]
  |} with
    | AssertionFail s -> print_string s
    | Eval.EvalFail s -> print_string s);
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
