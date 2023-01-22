open ParseUtil

exception ParseFail of string * CharStream.position
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
  | Def of (define * metadata)
and template = expr list
and var = {
  name: string;
  checks: (expr * metadata) list
}
and lam = {
  params: (var * metadata) list;
  lbody: expr list;
  env: expr Environment.t;
  f: lam_function
}
and lam_application = {
  lam: expr;
  args: expr list
}
and define = {
  dname: (var * metadata);
  dvalue: expr list
}
and lam_function = (expr list (*args*)
  -> (var * metadata) list (*params*)
  -> expr list (*lbody*)
  -> expr Environment.t (*closure*)
  -> expr Environment.t (*current env*)
  -> (expr Environment.t -> expr list -> expr)
  -> expr)

let rec exprToString (e: expr): string = match e with
  | Name (s, _) -> "Name(" ^ s ^ ")"
  | Var ({ name; checks }, _) -> funNotation "Var" (["name=" ^ name] @ (checks |> List.map fst |> List.map exprToString))
  | Asm (s, _) -> "Asm(" ^ s ^ ")"
  | ParsedAsm (asm, _) ->
      funNotation "ParsedAsm" [Assembly.asmToString asm]
  | Template (exprs, _) -> "Template(" ^ List.fold_left (^) "" (List.map exprToString exprs) ^ ")"
  | Lam (la, _) -> lamMuToString la
  | LamApplication ({ lam; args }, _) -> "LamApplication(lam=" ^
    exprToString lam ^ ", args=("
    ^ String.concat ", " (List.map exprToString args)
    ^ "))"
  | Def ({dname; dvalue}, _) -> funNotation "Def" [exprToString (Var dname); sequenceToString dvalue]
and sequenceToString seq = seq |> List.map exprToString |> String.concat "; "
and lamMuToString la =
  let { params; lbody; _ } = la in
  ("Lam(params=[" ^
    List.fold_left (^) "" (List.map exprToString (
      List.map (fun v -> Var (v, metaEmpty)) (List.map fst params)
    )) ^ "], lbody="
    ^ (sequenceToString lbody)
    ^ ")")
let metadata e =
  match e with
  | Name (_, metadata) -> metadata
  | Var (_, metadata) -> metadata
  | Asm (_, metadata) -> metadata
  | ParsedAsm (_, metadata) -> metadata
  | Template (_, metadata) -> metadata
  | Lam (_, metadata) -> metadata
  | LamApplication (_, metadata) -> metadata
  | Def (_, metadata) -> metadata
let locationToString e = CharStream.rangeToString (metadata e).r

let rec parseTopLevel std s =
  let token = CharStream.parseToken s in (
    match token with
    | Some (t, s', r) -> fst (parseExpr std t s' r)
    | None -> raise (ParseFail ("Expected list expression, not end-of-file", (s: CharStream.t).current))
  )
and parseList std startPos stream: expr * CharStream.t =
  let token = CharStream.parseToken stream in
    match token with
    | Some ("lam", s, _) -> parseLam std false startPos s
    | Some ("mu", s, _) -> parseLam std true startPos s
    | Some ("def", s, _) -> parseDef std startPos s
    | Some (t, s, r) -> parseLamApplication std (t, r) s
    | None -> raise (ParseFail ("Trailing '['", (stream: CharStream.t).current))
and parseLam std isMu startPos stream =
  let char = CharStream.consumeWhitespace stream in
    match char with
      | Some ('[', s, p) ->
        let p, _, (s': CharStream.t) = parseVarList std p [] s in
          let lbody, s'' = parseExprs std [] s' in
          let meta = metaInitial {startInclusive=startPos; endExclusive=s'.current} in
          Lam ({ params = p; lbody; env = Environment.empty; f = if isMu then Environment.find "mu" std else Environment.find "lam" std }, meta), s''
      | _ -> raise (ParseFail ("expected '['", (stream: CharStream.t).current))
and parseDef std startInclusive stream =
  let char = CharStream.consumeWhitespace stream in
  match char with
  | Some ('(', s, p) ->
    let dname, s' = parseVar std p s in
    let dvalue, (s'': CharStream.t) = parseExprs std [] s' in
    Def ({dname; dvalue}, metaInitial {startInclusive; endExclusive = s''.current}), s''
  | Some (bad, _, _) -> raise (ParseFail ("Expected '(', not " ^ (String.make 1 bad), (stream: CharStream.t).current))
  | None -> raise (ParseFail ("Expected Var, not end-of-file", (stream: CharStream.t).current))
and parseVar std startInclusive stream =
  let name = CharStream.parseToken stream in
    match name with
    | Some (")", _, _) -> raise (ParseFail ("Expected name, not ')'", (stream: CharStream.t).current))
    | Some (n, s, _) ->
      (let checks, (s': CharStream.t) = parseChecks std s in
        ({ name = n; checks = checks }, metaInitial {startInclusive; endExclusive=s'.current}),
        s')
    | None -> raise (ParseFail ("Expected name, not end-of-file", (stream: CharStream.t).current))
and parseTemplateRec std stream = let asm, meta, s = parseAsm stream in
  if asm <> "" then let rest, s' = parseTemplateRec std s in
    List.cons (Asm (asm, meta)) rest, s'
  else match CharStream.parseToken s with
  | Some ("}", s', _) -> [], s'
  | Some ("[", s', _) -> let e, s'' = parseList std s'.current s' in
    let rest, s''' = parseTemplateRec std s'' in
      List.cons e rest, s'''
  | Some _ -> raise (ParseFail ("This should be unreachable", (s: CharStream.t).current))
  | None -> raise (ParseFail ("Unexpected end-of-file before close of template", (s: CharStream.t).current))
and parseTemplate std startInclusive stream: template * CharStream.range * CharStream.t =
  let template, (s: CharStream.t) = parseTemplateRec std stream in
  template, {startInclusive=startInclusive; endExclusive=s.current}, s
and parseAsm stream =
  let pred = fun c -> not (List.mem c ['}';'[']) in
  let asm, r, s = CharStream.takeWhile pred stream in
    (List.fold_left (fun s c -> (String.make 1 c) ^ s) "" asm),
    metaInitial r,
    s
and parseExpr std t s (r: CharStream.range) =
  if t = "[" then parseList std r.startInclusive s
  else if t = "{" then let (template, r', s) = parseTemplate std r.startInclusive s in
  Template (template, metaInitial r'), s
  else Name (t, metaInitial r), s
and parseExprs std acc stream =
  match CharStream.parseToken stream with
  | Some ("]", s, _) -> acc, s
  | Some ("}", _, _) -> raise (ParseFail ("Expected expression or ], not }", (stream: CharStream.t).current))
  | Some (t, s, r) -> let e, s = parseExpr std t s r in parseExprs std (List.cons e acc) s
  | None -> raise (ParseFail ("Expected expression or ], not end-of-file", (stream: CharStream.t).current))
and parseChecks std s = parseChecksRec std [] s
and parseChecksRec std acc s =
  match CharStream.parseToken s with
  | Some (")", s', _) -> acc, s'
  | Some (t, s', r) -> let check, s'' = parseExpr std t s' r in
    let meta = metaInitial {startInclusive=r.startInclusive; endExclusive=s''.current} in
    parseChecksRec std (List.cons (check, meta) acc) s''
  | None -> raise (ParseFail ("Expected expression of ), not end-of-file", (s: CharStream.t).current))
and parseLamApplication std token stream =
  let t, r = token in
  let meta = metaInitial r in
  let lam, s' = (if t = "["
    then (parseList std r.startInclusive stream)
    else if Environment.mem t std then
      (CharStream.announceToken "function" "defaultLibrary" r;
      Lam ({params=[]; lbody=[]; env=Environment.empty; f=Environment.find t std}, meta), stream)
    else Name (t, meta), stream)
  in
    let args, (s'': CharStream.t) = parseArgs std s' [] in
    LamApplication (
      { lam = lam; args = List.rev args },
      metaInitial {startInclusive = r.startInclusive; endExclusive = s''.current}
    ), s''

and parseArgs std stream accumulator =
  let token = CharStream.parseToken stream in
  match token with
  | Some ("]", s, _) -> accumulator, s
  | Some (t, s, r) ->
    let nextExpr, s' =
      if t = "[" then parseList std r.startInclusive s
      else if t = "{" then let tem, r, ss' = parseTemplate std r.startInclusive s in Template (tem, metaInitial r), ss'
      else Name (t, metaInitial r), s
    in parseArgs std s' (nextExpr :: accumulator)
  | None -> raise (ParseFail ("Expected arg or ']', not end-of-file", (stream: CharStream.t).current))
and parseVarList std startInclusive accumulator stream =
  let c = CharStream.consumeWhitespace stream in
  match c with
  | Some (']', s, p) -> List.rev accumulator, (metaInitial {startInclusive; endExclusive=CharStream.incrementedCol p}), s
  | Some ('(', s, p) -> let v, s' = parseVar std p s in
    parseVarList std startInclusive (v :: accumulator) s'
  | Some (x, _, p) -> raise (ParseFail ("Expected ']' or '(', not " ^ String.make 1 x, p))
  | None -> raise (ParseFail ("Expected ']' or '(', not end-of-file", (stream: CharStream.t).current))
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
  | Lam _ -> thisIsUnevaluatedOrNotAssembly "lam" e
  | LamApplication _ -> thisIsUnevaluatedOrNotAssembly "unevaluated lam application" e
  | Def _ -> thisIsUnevaluatedOrNotAssembly "def" e
let handleParseFail runnable = try runnable () with e -> match e with
  | ParseFail (s, (p: CharStream.position)) -> print_endline (
    "Line " ^ (string_of_int (p.zeroBasedLine + 1)) ^ ", col " ^ (string_of_int (p.zeroBasedCol + 1)) ^ ": " ^ s); raise e
  | _ -> raise e
let parseFile std ic = parseTopLevel std (CharStream.inputChannelToSeq ic)
let testStd = Environment.empty
  |> Environment.add "lam" (fun _ _ _ _ _ _ -> Template ([], metaEmpty))
  |> Environment.add "mu" (fun _ _ _ _ _ _ -> Template ([], metaEmpty))
let getAst testStd text = text |> (CharStream.fromString CharStream.origin) |> parseTopLevel testStd
let printAst text: unit =
  text |> getAst testStd |> exprToString |> print_endline

let%expect_test _ =
printAst "[lam [] \"\" ]";
[%expect{| Lam(params=[], lbody=Name("")) |}]

let%expect_test _ =
  printAst "[lam [(a) (b) (c)] \"\" ]";
  [%expect{| Lam(params=[Var(name=a)Var(name=b)Var(name=c)], lbody=Name("")) |}]

let%expect_test _ =
  printAst "[a [lam [] test0] test1 ]";
  [%expect{| LamApplication(lam=Name(a), args=(Lam(params=[], lbody=Name(test0)), Name(test1))) |}]

let%expect_test _ =
  printAst "[a [lam [] [lam [] test0]] test1 ]";
  [%expect{| LamApplication(lam=Name(a), args=(Lam(params=[], lbody=Lam(params=[], lbody=Name(test0))), Name(test1))) |}]

let%expect_test _ =
  printAst "[[lam [] test2] test3 ]";
  [%expect{| LamApplication(lam=Lam(params=[], lbody=Name(test2)), args=(Name(test3))) |}]

let%expect_test _ =
  printAst "[f alpha {}]";
  [%expect {| LamApplication(lam=Name(f), args=(Name(alpha), Template())) |}]

let%expect_test _ = printAst "[[lam [(x)] {}] {} ]";
  [%expect{| LamApplication(lam=Lam(params=[Var(name=x)], lbody=Template()), args=(Template())) |}]

let%expect_test _ = handleParseFail (fun () -> printAst "[[lam [(x alpha [lam [(x)] x])] {}] {} ]");
  [%expect{| LamApplication(lam=Lam(params=[Var(name=x, Lam(params=[Var(name=x)], lbody=Name(x)), Name(alpha))], lbody=Template()), args=(Template())) |}]
