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
    | Some (c, s) -> if isWhitespace c then consumeWhitespace s else Some (c, s)
    | None -> None
let rec parseTokenRec stream current =
  let char = Seq.uncons stream in
    match char with
    | Some (c, s) ->
      if isWhitespace c
        then Some (current, s)
      else if List.mem c ['[';']';'(';')';'{';'}']
        then if current = ""
          then Some (String.make 1 c, s)
          else Some (current, Seq.cons c s)
      else parseTokenRec s (current ^ String.make 1 c)
    | None -> if current = "" then None else Some (current, stream)
let parseToken stream =
  match consumeWhitespace stream with
    | Some (c, s) -> parseTokenRec (Seq.cons c s) ""
    | None -> None

let inputChannelToSeq ic = Seq.of_dispenser (fun () -> (
  try Some (input_char ic) with
    End_of_file -> None
  ))


let rec parseTopLevel s acc =
  let char = Seq.uncons s in
    match char with
    | Some ('[', s') -> List.cons (parseList s') acc
    | Some (x, _) -> raise (ParseFail ("Expected '[', not " ^ String.make 1 x))
    | None -> acc
and parseList stream =
  let token = parseToken stream in
    match token with
    | Some ("lam", s) -> parseLam s
    | Some (t, s) -> parseLamApplication t s
    | None -> raise (ParseFail "Trailing '['")
and parseLam stream =
  let char = consumeWhitespace stream in
    match char with
      | Some ('[', s) ->
        let p, s' = parseVarList s [] in
          let value, s'' = parseExpr s' in (
            Lam { params = p; value = value },
            let t = parseToken s'' in
              match t with
              | Some ("]", s''') -> s'''
              | Some (tok, _) -> raise (ParseFail ("Expected \"]\" to close lam expression, but got " ^ tok ^ " instead"))
              | None -> raise (ParseFail "Unexpected end of file before close of lam expression")
          )
      | _ -> raise (ParseFail "expected '['")
and parseVar stream =
  let name = parseToken stream in
    match name with
    | Some (")", _) -> raise (ParseFail "Expected name, not ')'")
    | Some (n, s) -> let checks = parseChecks s in { name = n; checks = checks }
    | None -> raise (ParseFail "Expected name, not end-of-file")

and parseTemplate stream = let asm, s = parseAsm stream in [Asm asm], s
and parseAsm stream =
  let pred = fun c -> c != '}' in
    let asm = Seq.take_while pred stream in
      (Seq.fold_left (fun s c -> s ^ (String.make 1 c)) "" asm),
      Seq.drop 1 (Seq.drop_while pred stream)
and parseExpr stream = let token = parseToken stream in
  match token with
  | Some ("{", s) -> let template, s = parseTemplate s in Template template, s
  | Some ("[", s) -> parseList s
  | None -> raise (ParseFail "Expected expression, not end-of-file")
  | Some (t, s) -> Name t, s
and parseChecks _ = []
and parseLamApplication token stream =
  let lam, s' = if token = "[" then parseList stream else Name token, stream in
    let args, s'' = parseArgs s' [] in
    LamApplication { lam = lam; args = List.rev args }, s''

and parseArgs stream accumulator =
  let token = parseToken stream in
  match token with
  | Some ("]", s) -> accumulator, s
  | Some (t, s) ->
    let nextExpr, s' =
      if t = "[" then parseList s
      else if t = "{" then let tem, ss' = parseTemplate s in Template tem, ss'
      else Name t, s
    in parseArgs s' (nextExpr :: accumulator)
  | None -> raise (ParseFail "Expected arg or ']', not end-of-file")
and parseVarList stream accumulator =
  let c = consumeWhitespace stream in
  match c with
  | Some (']', s) -> accumulator, s
  | Some ('(', s) -> parseVarList s ((parseVar s) :: accumulator)
  | Some (x, _) -> raise (ParseFail ("Expected ']' or '(', not " ^ String.make 1 x))
  | None -> raise (ParseFail "Expected ']' or '(', not end-of-file")

let parseFile ic = parseTopLevel (inputChannelToSeq ic) []

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
  | LamApplication { lam; args } -> "LamApplication(lam=" ^
    exprToString lam ^ ", args=("
    ^ String.concat ", " (List.map exprToString args)
    ^ "))"

let printAst text: unit = List.iter print_endline (List.map exprToString (
  List.map fst
  ( parseTopLevel (String.to_seq text) [] )
))

let%expect_test _ =
  printAst "[lam [] \"\" ]";
  [%expect{| Lam(params=[], value=Name("")) |}]

let%expect_test _ =
  printAst "[a [lam [] test0] test1 ]";
  [%expect{| LamApplication(lam=Name(a), args=(Lam(params=[], value=Name(test0)), Name(test1))) |}]

let%expect_test _ =
  printAst "[a [lam [] [lam [] test0]] test1 ]";
  [%expect{| LamApplication(lam=Name(a), args=(Lam(params=[], value=Lam(params=[], value=Name(test0))), Name(test1))) |}]

let%expect_test _ =
  printAst "[[lam [] test2] test3 ]";
  [%expect{| LamApplication(lam=Lam(params=[], value=Name(test2)), args=(Name(test3))) |}]

let%expect_test _ =
  printAst "[f alpha {}]";
  [%expect {| LamApplication(lam=Name(f), args=(Name(alpha), Template(Asm()))) |}]
