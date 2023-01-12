open ParseUtil

exception ParseFail of string
module Environment = Map.Make(String)

type expr =
  | Name of string
  | Var of var
  | Asm of string
  | ParsedAsm of Assembly.t
  | Template of template
  | Lam of lam
  | LamApplication of lam_application
and template = expr list
and var = {
  name: string;
  checks: lam list
}
and lam = {
  params: var list;
  value: expr;
  env: expr Environment.t
}
and lam_application = {
  lam: expr;
  args: expr list
}

let rec exprToString (e: expr): string = match e with
  | Name s -> "Name(" ^ s ^ ")"
  | Var { name; _ } -> "Var(name=" ^ name ^ ")"
  | Asm s -> "Asm(" ^ s ^ ")"
  | ParsedAsm { top; middle; bottom } ->
    let middle = middle |> List.map Assembly.instrToString |> String.concat "\n" in
      funNotation "ParsedAsm" [top;middle;bottom]
  | Template exprs -> "Template(" ^ List.fold_left (^) "" (List.map exprToString exprs) ^ ")"
  | Lam { params; value; _ } -> "Lam(params=[" ^
    List.fold_left (^) "" (List.map exprToString (List.map (fun v -> Var v) params)) ^ "], value="
    ^ (exprToString value)
    ^ ")"
  | LamApplication { lam; args } -> "LamApplication(lam=" ^
    exprToString lam ^ ", args=("
    ^ String.concat ", " (List.map exprToString args)
    ^ "))"


let rec parseTopLevel acc s =
  List.map fst (
    let cs = consumeWhitespace s in (
      match cs with
      | Some ('[', s') -> List.cons (parseList s') acc
      | Some (x, _) -> raise (ParseFail ("Expected '[', not " ^ String.make 1 x))
      | None -> acc
  ))
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
        let p, s' = parseVarList [] s in
          let value, s'' = parseExpr s' in (
            Lam { params = p; value = value; env = Environment.empty },
              match parseToken s'' with
              | Some ("]", s''') -> s'''
              | Some (tok, _) -> raise (ParseFail ("Expected \"]\" to close lam expression, but got " ^ tok ^ " instead"))
              | None -> raise (ParseFail "Unexpected end of file before close of lam expression")
          )
      | _ -> raise (ParseFail "expected '['")
and parseVar stream =
  let name = parseToken stream in
    match name with
    | Some (")", _) -> raise (ParseFail "Expected name, not ')'")
    | Some (n, s) -> let checks, s' = parseChecks s in { name = n; checks = checks }, (
        match parseToken s' with
        | Some(")", s'') -> s''
        | Some(bad, _) -> raise (ParseFail ("Expected ')', not " ^ bad))
        | None -> raise (ParseFail "Expected ')', not end-of-file")
      )
    | None -> raise (ParseFail "Expected name, not end-of-file")

and parseTemplate stream = let asm, s = parseAsm stream in
  if asm <> "" then let rest, s' = parseTemplate s in
    List.cons (Asm asm) rest, s'
  else match parseToken s with
  | Some ("}", s') -> [], s'
  | Some ("[", s') -> let e, s'' = parseList s' in
    let rest, s''' = parseTemplate s'' in
      List.cons e rest, s'''
  | Some _ -> raise (ParseFail "This should be unreachable")
  | None -> raise (ParseFail "Unexpected end-of-file before close of template")
and parseAsm stream =
  let pred = fun c -> not (List.mem c ['}';'[']) in
    let asm, s = takeWhile pred stream in
      (List.fold_left (fun s c -> (String.make 1 c) ^ s) "" asm),
      s
and parseExpr stream = let token = parseToken stream in
  match token with
  | Some ("{", s) -> let template, s = parseTemplate s in Template template, s
  | Some ("[", s) -> parseList s
  | None -> raise (ParseFail "Expected expression, not end-of-file")
  | Some (t, s) -> Name t, s
and parseChecks s = [], s
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
and parseVarList accumulator stream =
  let c = consumeWhitespace stream in
  match c with
  | Some (']', s) -> accumulator, s
  | Some ('(', s) -> let v, s' = parseVar s in
    parseVarList (v :: accumulator) s'
  | Some (x, _) -> raise (ParseFail ("Expected ']' or '(', not " ^ String.make 1 x))
  | None -> raise (ParseFail "Expected ']' or '(', not end-of-file")
let thisIsUnevaluatedOrNotAssembly description e =
  raise (Assembly.AsmParseFail ("Attempted to parse " ^ description ^ " " ^ exprToString e ^ " as assembly"))
let rec exprToParsedAsm env e =
  match e with
  | Name _ -> thisIsUnevaluatedOrNotAssembly "unbound name" e
  | Var _ -> thisIsUnevaluatedOrNotAssembly "variable declaration" e
  | Asm s -> Assembly.parse Assembly.empty env s
  | ParsedAsm a -> a
  | Template tem -> tem |> List.map (exprToParsedAsm env) |> List.fold_left (
    fun acc (next: Assembly.t) -> let spliced = Assembly.parse acc env next.top in (
      if String.trim spliced.bottom <> "" then (raise (Assembly.AsmParseFail (
        "The blocks of assembly " ^ (Assembly.asmToString acc) ^ " and " ^ (Assembly.asmToString next) ^ "Do not combine to form valid assembly"
      ))) else {
        top = spliced.top; middle = spliced.middle @ next.middle; bottom = next.bottom
      }
    )
  ) Assembly.empty
  | Lam _ -> thisIsUnevaluatedOrNotAssembly "lam definition" e
  | LamApplication _ -> thisIsUnevaluatedOrNotAssembly "unevaluated lam application" e

let parseFile ic = parseTopLevel [] (inputChannelToSeq ic)

let getAst text = text |> String.to_seq |> (parseTopLevel [])
let printAst text: unit =
  text |> getAst |> List.map exprToString |> List.iter print_endline

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
  [%expect {| LamApplication(lam=Name(f), args=(Name(alpha), Template())) |}]

let%expect_test _ = printAst "[[lam [(x)] {}] {} ]";
  [%expect{| LamApplication(lam=Lam(params=[Var(name=x)], value=Template()), args=(Template())) |}]
