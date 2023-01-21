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

let std: Ast.lam_function E.t = E.empty
  |> E.add "print" print
  |> E.add "fail" fail
  |> E.add "lam" Eval.lam
  |> E.add "mu" Eval.mu

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
