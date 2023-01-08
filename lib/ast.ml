exception ParseFail of string

type expr =
  | Name of string
  | Var of var
  | Asm of string
  | Template of template
  | ExprList of expr list
  | Lam of lam
  | LamApplication of lam_application
and template = expr list
and var = {
  name: string;
  checks: lam list (* checks executed dynamically when var is bound *)
}
and lam = {
  params: var list;
  value: expr
}
and lam_application = {
  lam: expr;
  args: expr list
}

let isWhitespace char = List.mem char [' '; '\n'; '\t'; '\r']
let rec consumeWhitespace stream =
  let char = Seq.uncons stream in
    match char with
    | Some (c, _) -> if isWhitespace c then consumeWhitespace stream else Some (String.make 1 c)
    | None -> None
let rec parseTokenRec stream current =
  let char = Seq.uncons stream in
    match char with
    | Some (c, _) ->
      if isWhitespace c then Some current else parseTokenRec stream (current ^ String.make 1 c)
    | None -> if current == "" then None else Some current
let parseToken stream =
  match consumeWhitespace stream with
    | Some c -> parseTokenRec stream c
    | None -> None

let rec parseTopLevel stream acc =
  let s = Seq.of_dispenser (fun () -> (
    try Some (input_char stream) with
      End_of_file -> None
  )) in
    let char = Seq.uncons s in
      match char with
      | Some ('[', _) -> List.cons (parseList s) acc
      | Some (x, _) -> raise (ParseFail ("Expected '[', not " ^ String.make 1 x))
      | None -> acc
and parseList stream : expr =
  let token = parseToken stream in
    match token with
    | Some "lam" -> parseLam stream
    | Some t -> parseLamApplication t stream
    | None -> raise (ParseFail "Trailing '['")
and parseLam stream =
  let char = consumeWhitespace stream in
    let p = (
      match char with
      | Some "[" -> parseVarList stream []
      | _ -> raise (ParseFail "expected '['")
     ) in
      Lam { params = p; value = parseExpr stream }
and parseVar stream =
  let name = parseToken stream in
    match name with
    | Some ")" -> raise (ParseFail "Expected name, not ')'")
    | Some n -> let checks = parseChecks stream in { name = n; checks = checks }
    | None -> raise (ParseFail "Expected name, not end-of-file")

and parseTemplate stream = [Asm (parseAsm stream)]
and parseAsm stream = let asm = Seq.take_while (fun c -> c != '}') stream in
  Seq.fold_left (fun s c -> s ^ (String.make 1 c)) "" asm
  (* let char = input_char stream in
    if char = '}' then acc else acc ^ (String.make 1 char) *)
and parseExpr stream = let token = parseToken stream in
  match token with
  | Some "{" -> Template (parseTemplate stream)
  | Some "[" -> parseList stream
  | None -> raise (ParseFail "Expected expression, not end-of-file")
  | Some t -> Name t
and parseChecks _ = []
and parseLamApplication token stream = LamApplication {
  lam = if token = "[" then parseList stream else Name token;
  args = parseArgs stream []
}
and parseArgs stream accumulator =
  let token = parseToken stream in
  match token with
  | Some "]" -> accumulator
  | Some t -> parseArgs stream (Name t :: accumulator)
  | None -> raise (ParseFail "Expected arg or ']', not end-of-file")
and parseVarList stream accumulator =
  let c = consumeWhitespace stream in
  match c with
  | Some "]" -> accumulator
  | Some "(" -> parseVarList stream ((parseVar stream) :: accumulator)
  | Some x -> raise (ParseFail ("Expected ']' or '(', not " ^ x))
  | None -> raise (ParseFail "Expected ']' or '(', not end-of-file")

let rec exprToString (e: expr): string = match e with
  | Name s -> "Name(" ^ s ^ ")"
  | Var { name; _ } -> "Var(name=" ^ name ^ ")"
  | Asm s -> "Asm(" ^ s ^ ")"
  | Template exprs -> "Template(" ^ List.fold_left (^) "" (List.map exprToString exprs) ^ ")"
  | ExprList exprs -> "ExprList(" ^ List.fold_left (^) "" (List.map exprToString exprs) ^ ")"
  | Lam { params; value } -> "Lam(params=[" ^
    List.fold_left (^) "" (List.map exprToString (List.map (fun v -> Var v) params)) ^ "], value="
    ^ (exprToString value)
    ^ ")"
  | LamApplication { lam; args } -> "LamApplication(" ^
    exprToString lam ^ ", "
    ^ List.fold_left (^) "" (List.map exprToString args)
    ^ ")"
