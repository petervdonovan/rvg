open ParseUtil

exception ParseFail of string
module Environment = Map.Make(String)
module Attributes = Set.Make(String)

type metadata = {
  attrs: Attributes.t;
  r: CharStream.range;
}
let metaInitial r = {attrs=Attributes.empty;r}
let metaEmpty = {
  attrs = Attributes.empty;
  r = {
    startInclusive = {zeroBasedLine = 0; zeroBasedCol = 0};
    endExclusive = {zeroBasedLine = 0; zeroBasedCol = 0};
  }
}
type expr =
  | Name of (string * metadata)
  | Var of (var * metadata)
  | Asm of (string * metadata)
  | ParsedAsm of (Assembly.t * metadata)
  | Template of (template * metadata)
  | Lam of (lam * metadata)
  | LamApplication of (lam_application * metadata)
and template = expr list
and var = {
  name: string;
  checks: (lam * metadata) list
}
and lam = {
  params: (var * metadata) list;
  value: expr;
  env: expr Environment.t
}
and lam_application = {
  lam: expr;
  args: expr list
}

let rec exprToString (e: expr): string = match e with
  | Name (s, _) -> "Name(" ^ s ^ ")"
  | Var ({ name; _ }, _) -> "Var(name=" ^ name ^ ")"
  | Asm (s, _) -> "Asm(" ^ s ^ ")"
  | ParsedAsm (asm, _) ->
      funNotation "ParsedAsm" [Assembly.asmToString asm]
  | Template (exprs, _) -> "Template(" ^ List.fold_left (^) "" (List.map exprToString exprs) ^ ")"
  | Lam ({ params; value; _ }, _) -> "Lam(params=[" ^
    List.fold_left (^) "" (List.map exprToString (
      List.map (fun v -> Var (v, metaEmpty)) (List.map fst params)
    )) ^ "], value="
    ^ (exprToString value)
    ^ ")"
  | LamApplication ({ lam; args }, _) -> "LamApplication(lam=" ^
    exprToString lam ^ ", args=("
    ^ String.concat ", " (List.map exprToString args)
    ^ "))"


let rec parseTopLevel acc s =
  List.map fst (
    let cs = CharStream.consumeWhitespace s in (
      match cs with
      | Some ('[', s', p) -> List.cons (parseList p s') acc
      | Some (x, _, _) -> raise (ParseFail ("Expected '[', not " ^ String.make 1 x))
      | None -> acc
  ))
and parseList startPos stream =
  let token = CharStream.parseToken stream in
    match token with
    | Some ("lam", s, _) -> parseLam startPos s
    | Some (t, s, r) -> parseLamApplication (t, r) s
    | None -> raise (ParseFail "Trailing '['")
and parseLam startPos stream =
  let char = CharStream.consumeWhitespace stream in
    match char with
      | Some ('[', s, p) ->
        let p, _, s' = parseVarList p [] s in
          let value, s'' = parseExpr s' in (
            match CharStream.parseToken s'' with
            | Some ("]", s''', r) -> Lam (
              { params = p; value = value; env = Environment.empty },
              metaInitial {startInclusive=startPos; endExclusive=r.endExclusive}
              ), s'''
            | Some (tok, _, _) -> raise (ParseFail ("Expected \"]\" to close lam expression, but got " ^ tok ^ " instead"))
            | None -> raise (ParseFail "Unexpected end of file before close of lam expression")
          )
      | _ -> raise (ParseFail "expected '['")
and parseVar startInclusive stream =
  let name = CharStream.parseToken stream in
    match name with
    | Some (")", _, _) -> raise (ParseFail "Expected name, not ')'")
    | Some (n, s, _) ->
      let checks, s' = parseChecks s in (
        match CharStream.parseToken s' with
        | Some(")", s'', r') -> (
          { name = n; checks = checks },
          metaInitial {startInclusive; endExclusive=r'.endExclusive}),
          s''
        | Some(bad, _, _) -> raise (ParseFail ("Expected ')', not " ^ bad))
        | None -> raise (ParseFail "Expected ')', not end-of-file")
      )
    | None -> raise (ParseFail "Expected name, not end-of-file")

and parseTemplateRec stream = let asm, meta, s = parseAsm stream in
  if asm <> "" then let rest, s' = parseTemplateRec s in
    List.cons (Asm (asm, meta)) rest, s'
  else match CharStream.parseToken s with
  | Some ("}", s', _) -> [], s'
  | Some ("[", s', _) -> let e, s'' = parseList s'.current s' in
    let rest, s''' = parseTemplateRec s'' in
      List.cons e rest, s'''
  | Some _ -> raise (ParseFail "This should be unreachable")
  | None -> raise (ParseFail "Unexpected end-of-file before close of template")
and parseTemplate startInclusive stream: template * CharStream.range * CharStream.t =
  let template, (s: CharStream.t) = parseTemplateRec stream in
  template, {startInclusive=startInclusive; endExclusive=s.current}, s
and parseAsm stream =
  let pred = fun c -> not (List.mem c ['}';'[']) in
    let asm, r, s = CharStream.takeWhile pred stream in
      (List.fold_left (fun s c -> (String.make 1 c) ^ s) "" asm),
      metaInitial r,
      s
and parseExpr stream = let token = CharStream.parseToken stream in
  match token with
  | Some ("{", s, r) -> let (template, r', s) = parseTemplate r.startInclusive s in
    Template (template, metaInitial r'), s
  | Some ("[", s, r) -> parseList r.startInclusive s
  | Some (t, s, r) -> Name (t, metaInitial r), s
  | None -> raise (ParseFail "Expected expression, not end-of-file")
and parseChecks s = [], s
and parseLamApplication token stream =
  let t, r = token in
  let lam, s' = (if t = "["
    then (parseList r.startInclusive stream)
    else Name (t, metaInitial r), stream)
  in
    let args, (s'': CharStream.t) = parseArgs s' [] in
    LamApplication (
      { lam = lam; args = List.rev args },
      metaInitial {startInclusive = r.startInclusive; endExclusive = s''.current}
    ), s''

and parseArgs stream accumulator =
  let token = CharStream.parseToken stream in
  match token with
  | Some ("]", s, _) -> accumulator, s
  | Some (t, s, r) ->
    let nextExpr, s' =
      if t = "[" then parseList r.startInclusive s
      else if t = "{" then let tem, r, ss' = parseTemplate r.startInclusive s in Template (tem, metaInitial r), ss'
      else Name (t, metaInitial r), s
    in parseArgs s' (nextExpr :: accumulator)
  | None -> raise (ParseFail "Expected arg or ']', not end-of-file")
and parseVarList startInclusive accumulator stream =
  let c = CharStream.consumeWhitespace stream in
  match c with
  | Some (']', s, p) -> List.rev accumulator, (metaInitial {startInclusive; endExclusive=CharStream.incrementedCol p}), s
  | Some ('(', s, p) -> let v, s' = parseVar p s in
    parseVarList startInclusive (v :: accumulator) s'
  | Some (x, _, _) -> raise (ParseFail ("Expected ']' or '(', not " ^ String.make 1 x))
  | None -> raise (ParseFail "Expected ']' or '(', not end-of-file")
let thisIsUnevaluatedOrNotAssembly description e =
  raise (Assembly.AsmParseFail ("Attempted to parse " ^ description ^ " " ^ exprToString e ^ " as assembly"))
let rec exprToParsedAsm env e =
  match e with
  | Name _ -> thisIsUnevaluatedOrNotAssembly "unbound name" e
  | Var _ -> thisIsUnevaluatedOrNotAssembly "variable declaration" e
  | Asm (s, meta) -> Assembly.parse Assembly.empty env s, meta
  | ParsedAsm (a, meta) -> a, meta
  | Template (tem, meta) -> (tem
    |> List.map (exprToParsedAsm env)
    |> List.map fst
    |> List.rev (* FIXME: Performance issue? *)
    |> List.fold_left (Assembly.prependBlock env) {top=""; middle = []; bottom = ""}
    |> Assembly.promoteOrDemote env,
    meta)
  | Lam _ -> thisIsUnevaluatedOrNotAssembly "lam definition" e
  | LamApplication _ -> thisIsUnevaluatedOrNotAssembly "unevaluated lam application" e

let parseFile ic = parseTopLevel [] (CharStream.inputChannelToSeq ic)

let getAst text = text |> (CharStream.fromString CharStream.origin) |> (parseTopLevel [])
let printAst text: unit =
  text |> getAst |> List.map exprToString |> List.iter print_endline

let%expect_test _ =
printAst "[lam [] \"\" ]";
[%expect{| Lam(params=[], value=Name("")) |}]

let%expect_test _ =
  printAst "[lam [(a) (b) (c)] \"\" ]";
  [%expect{| Lam(params=[Var(name=a)Var(name=b)Var(name=c)], value=Name("")) |}]

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
