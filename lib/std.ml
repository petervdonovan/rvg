module E = Map.Make(String)

exception IllegalArgument of string
exception WrongNumberOfArgs of string

let assertNArgs n args: unit =
  let len = List.length args in
  if len <> n
    then raise (WrongNumberOfArgs ("Expected " ^ (string_of_int n) ^ " arguments, but got " ^ (string_of_int len) ^ " arguments."))
    else ()

let print args = (
  assertNArgs 1 args;
  let arg = List.hd args in
  (match arg with
  | Ast.ParsedAsm (pasm, _) -> Assembly.print pasm; arg
  | _ -> raise (IllegalArgument ("Expected ParsedAsm, but got " ^ Ast.exprToString arg)))
)

let (functions: (Ast.expr list -> Ast.expr) E.t) = E.empty
  |> E.add "print" print
