open ParseUtil

exception ParseFail of string * CharStream.position

module Environment = Map.Make (String)
module Attributes = Set.Make (String)

type metadata = {attrs: Attributes.t; r: CharStream.range}

let metaInitial r = {attrs= Attributes.empty; r}

let metaEmpty =
  { attrs= Attributes.empty
  ; r=
      { startInclusive= {zeroBasedLine= 0; zeroBasedCol= 0; file= ""}
      ; endExclusive= {zeroBasedLine= 0; zeroBasedCol= 0; file= ""} } }

type expr_content =
  | Name of string
  | Var of var
  | Asm of string
  | ParsedAsm of Assembly.t
  | Template of template
  | Lam of lam
  | LamApplication of lam_application
  | Def of define
  | Integer of int

and expr = expr_content * metadata

and template = expr list * expr Environment.t

and var = {name: string; checks: (expr * metadata) list}

and param = PVar of var | Word of string

and lam =
  {params: (param * metadata) list; lbody: expr list; env: expr Environment.t; f: lam_function}

and lam_application = {lam: expr; args: expr list}

and define = {dname: var * metadata; dvalue: expr list}

and lam_function =
     expr list (*args*)
  -> (param * metadata) list (*params*)
  -> expr list (*lbody*)
  -> expr Environment.t (*closure*)
  -> expr Environment.t (*current env*)
  -> (expr Environment.t -> expr list -> expr)
  -> (*evalSequence*)
     CharStream.range (*lam application expression range*)
  -> expr

let rec exprContentToString (e : expr_content) : string =
  match e with
  | Name s ->
      "Name(" ^ s ^ ")"
  | Var {name; checks} ->
      funNotation "Var" (["name=" ^ name] @ (checks |> List.map fst |> List.map exprToString))
  | Asm s ->
      "Asm(" ^ s ^ ")"
  | ParsedAsm (FinishedBlock asm) ->
      funNotation "ParsedAsmF" [Assembly.asmToStringInternal (FinishedBlock asm)]
  | ParsedAsm asm ->
      funNotation "ParsedAsm" [Assembly.asmToStringInternal asm]
  | Template (exprs, _) ->
      "Template(" ^ List.fold_left ( ^ ) "" (List.map exprToString exprs) ^ ")"
  | Lam la ->
      lamMuToString la
  | LamApplication {lam; args} ->
      "LamApplication(lam=" ^ exprToString lam ^ ", args=("
      ^ String.concat ", " (List.map exprToString args)
      ^ "))"
  | Def {dname; dvalue} ->
      funNotation "Def" [exprToString (Var (fst dname), snd dname); sequenceToString dvalue]
  | Integer i ->
      Int.to_string i

and exprToString (e : expr) : string =
  let content, metadata = e in
  funNotation "E"
    [exprContentToString content; String.concat ";" (Attributes.elements metadata.attrs)]

and sequenceToString seq = seq |> List.map exprToString |> String.concat "; "

and lamMuToString la =
  let {params; lbody; _} = la in
  "Lam(params=["
  ^ List.fold_left ( ^ ) ""
      (List.map exprToString
         ( params |> List.map fst
         |> List.filter_map (fun p -> match p with PVar p -> Some p | Word _ -> None)
         |> List.map (fun v -> (Var v, metaEmpty)) ) )
  ^ "], lbody=" ^ sequenceToString lbody ^ ")"

let rangeOf e = (snd e).r

let locationToString e = CharStream.rangeToString (rangeOf e)

let rec parseTopLevel std s =
  let token = CharStream.parseToken s in
  match token with
  | Some (t, s', r) ->
      fst (parseExpr std t s' r)
  | None ->
      raise (ParseFail ("Expected list expression, not end-of-file", (s : CharStream.t).current))

and parseList std startPos stream : expr * CharStream.t =
  let token = CharStream.parseToken stream in
  match token with
  | Some ("lam", s, _) ->
      parseLam std false startPos s
  | Some ("mu", s, _) ->
      parseLam std true startPos s
  | Some ("def", s, _) ->
      parseDef std startPos s
  | Some (t, s, r) ->
      parseLamApplication std (t, r) s
  | None ->
      raise (ParseFail ("Trailing '['", (stream : CharStream.t).current))

and parseLam std isMu startPos stream =
  let char = CharStream.consumeWhitespace stream in
  match char with
  | Some ('[', s, p) ->
      let p, _, (s' : CharStream.t) = parseVarList std p [] s in
      let lbody, s'' = parseExprs std [] s' in
      let meta = metaInitial {startInclusive= startPos; endExclusive= s'.current} in
      ( ( Lam
            { params= p
            ; lbody
            ; env= Environment.empty
            ; f= (if isMu then Environment.find "mu" std else Environment.find "lam" std) }
        , meta )
      , s'' )
  | _ ->
      raise (ParseFail ("expected '['", (stream : CharStream.t).current))

and parseDef std startInclusive stream =
  let char = CharStream.consumeWhitespace stream in
  match char with
  | Some ('(', s, p) ->
      let dname, s' = parseVar std p s in
      let dvalue, (s'' : CharStream.t) = parseExprs std [] s' in
      ((Def {dname; dvalue}, metaInitial {startInclusive; endExclusive= s''.current}), s'')
  | Some (bad, _, _) ->
      raise (ParseFail ("Expected '(', not " ^ String.make 1 bad, (stream : CharStream.t).current))
  | None ->
      raise (ParseFail ("Expected Var, not end-of-file", (stream : CharStream.t).current))

and parseVar std startInclusive stream =
  let name = CharStream.parseToken stream in
  match name with
  | Some (")", _, _) ->
      raise (ParseFail ("Expected name, not ')'", (stream : CharStream.t).current))
  | Some (n, s, _) ->
      let checks, (s' : CharStream.t) = parseChecks std s in
      (({name= n; checks}, metaInitial {startInclusive; endExclusive= s'.current}), s')
  | None ->
      raise (ParseFail ("Expected name, not end-of-file", (stream : CharStream.t).current))

and parseTemplateRec std marker stream =
  let asm, meta, s = parseAsm marker stream in
  if asm <> "" then
    let rest, s' = parseTemplateRec std marker s in
    (List.cons (Asm asm, meta) rest, s')
  else
    match CharStream.parseToken s with
    | Some ("}", s', _) ->
        ([], s')
    | Some ("[", s', _) ->
        let e, s'' = parseList std s'.current s' in
        let rest, s''' = parseTemplateRec std marker s'' in
        (List.cons e rest, s''')
    | Some _ ->
        raise (ParseFail ("This should be unreachable", (s : CharStream.t).current))
    | None ->
        raise
          (ParseFail ("Unexpected end-of-file before close of template", (s : CharStream.t).current))

and parseTemplate std startInclusive stream : template * CharStream.range * CharStream.t =
  let marker, s' =
    match CharStream.uncons stream with
    | Some (c, s') ->
        if c = '|' then ("|", s') else ("", CharStream.cons c s')
    | None ->
        ("", stream)
  in
  let tem, (s : CharStream.t) = parseTemplateRec std marker s' in
  ((tem, Environment.empty), {startInclusive; endExclusive= s.current}, s)

and parseAsm marker stream =
  let pred c = not (List.mem c ['}'; '[']) in
  let contents, r, s = CharStream.takeWhile pred stream in
  if contents = "" then (contents, metaInitial r, s)
  else if String.ends_with ~suffix:marker contents then
    (String.sub contents 0 (String.length contents - String.length marker), metaInitial r, s)
  else
    match CharStream.uncons s with
    | Some (c, s') ->
        let rest, r', s'' = parseAsm marker s' in
        ( contents ^ String.make 1 c ^ rest
        , metaInitial {startInclusive= r.startInclusive; endExclusive= r'.r.endExclusive}
        , s'' )
    | None ->
        (contents, metaInitial r, s)

and parseExpr std t s (r : CharStream.range) =
  if t = "[" then parseList std r.startInclusive s
  else if String.starts_with ~prefix:"{" t then
    let template, r', s = parseTemplate std r.startInclusive s in
    ((Template template, metaInitial r'), s)
  else
    match int_of_string_opt t with
    | Some i ->
        ((Integer i, metaInitial r), s)
    | None ->
        ((Name t, metaInitial r), s)

and parseExprs std acc stream =
  match CharStream.parseToken stream with
  | Some ("]", s, _) ->
      (acc, s)
  | Some ("}", _, _) ->
      raise (ParseFail ("Expected expression or ], not }", (stream : CharStream.t).current))
  | Some (t, s, r) ->
      let e, s = parseExpr std t s r in
      parseExprs std (List.cons e acc) s
  | None ->
      raise
        (ParseFail ("Expected expression or ], not end-of-file", (stream : CharStream.t).current))

and parseChecks std s = parseChecksRec std [] s

and parseChecksRec std acc s =
  match CharStream.parseToken s with
  | Some (")", s', _) ->
      (acc, s')
  | Some (t, s', r) ->
      let check, s'' = parseExpr std t s' r in
      let meta = metaInitial {startInclusive= r.startInclusive; endExclusive= s''.current} in
      parseChecksRec std (List.cons (check, meta) acc) s''
  | None ->
      raise (ParseFail ("Expected expression or ), not end-of-file", (s : CharStream.t).current))

and parseLamApplication std token stream =
  let t, r = token in
  let meta = metaInitial r in
  let lam, s' =
    if t = "[" then parseList std r.startInclusive stream else ((Name t, meta), stream)
  in
  let args, (s'' : CharStream.t) = parseArgs std s' [] in
  ( ( LamApplication {lam; args= List.rev args}
    , metaInitial {startInclusive= r.startInclusive; endExclusive= s''.current} )
  , s'' )

and parseArgs std stream accumulator =
  let token = CharStream.parseToken stream in
  match token with
  | Some ("]", s, _) ->
      (accumulator, s)
  | Some (t, s, r) ->
      let nextExpr, s' = parseExpr std t s r in
      parseArgs std s' (nextExpr :: accumulator)
  | None ->
      raise (ParseFail ("Expected arg or ']', not end-of-file", (stream : CharStream.t).current))

and parseVarList std startInclusive accumulator stream =
  let c = CharStream.consumeWhitespace stream in
  match c with
  | Some (']', s, p) ->
      ( List.rev accumulator
      , metaInitial {startInclusive; endExclusive= CharStream.incrementedCol p}
      , s )
  | Some ('(', s, p) ->
      let (v, m), s' = parseVar std p s in
      parseVarList std startInclusive ((PVar v, m) :: accumulator) s'
  | Some (c, s, p) -> (
    match CharStream.parseToken (CharStream.cons c s) with
    | None ->
        raise (ParseFail ("unreachable", p))
    | Some (w, s', r) ->
        parseVarList std startInclusive ((Word w, metaInitial r) :: accumulator) s' )
  | None ->
      raise (ParseFail ("Expected ']' or '(', not end-of-file", (stream : CharStream.t).current))

let rec unwrap e =
  match e with
  | Template (tem, _) ->
      if List.length tem = 1 then tem |> List.hd |> fst |> unwrap else None
  | Asm s ->
      Some s
  | _ ->
      None

let handleParseFail runnable =
  try runnable ()
  with e -> (
    match e with
    | ParseFail (s, (p : CharStream.position)) ->
        print_endline
          ( "Line "
          ^ string_of_int (p.zeroBasedLine + 1)
          ^ ", col "
          ^ string_of_int (p.zeroBasedCol + 1)
          ^ ": " ^ s ) ;
        raise e
    | _ ->
        raise e )

let testStd =
  Environment.empty
  |> Environment.add "lam" (fun _ _ _ _ _ _ _ -> (Template ([], Environment.empty), metaEmpty))
  |> Environment.add "mu" (fun _ _ _ _ _ _ _ -> (Template ([], Environment.empty), metaEmpty))

let getAst testStd text = text |> CharStream.fromString "" |> parseTopLevel testStd

let printAst text : unit = text |> getAst testStd |> exprToString |> print_endline

let%expect_test _ =
  printAst "[lam [] \"\" ]" ; [%expect {| E(Lam(params=[], lbody=E(Name(""), )), ) |}]

let%expect_test _ =
  printAst "[lam [(a) (b) (c)] \"\" ]" ;
  [%expect
    {| E(Lam(params=[E(Var(name=a), )E(Var(name=b), )E(Var(name=c), )], lbody=E(Name(""), )), ) |}]

let%expect_test _ =
  printAst "[a [lam [] test0] test1 ]" ;
  [%expect
    {| E(LamApplication(lam=E(Name(a), ), args=(E(Lam(params=[], lbody=E(Name(test0), )), ), E(Name(test1), ))), ) |}]

let%expect_test _ =
  printAst "[a [lam [] [lam [] test0]] test1 ]" ;
  [%expect
    {| E(LamApplication(lam=E(Name(a), ), args=(E(Lam(params=[], lbody=E(Lam(params=[], lbody=E(Name(test0), )), )), ), E(Name(test1), ))), ) |}]

let%expect_test _ =
  printAst "[[lam [] test2] test3 ]" ;
  [%expect
    {| E(LamApplication(lam=E(Lam(params=[], lbody=E(Name(test2), )), ), args=(E(Name(test3), ))), ) |}]

let%expect_test _ =
  printAst "[f alpha {}]" ;
  [%expect {| E(LamApplication(lam=E(Name(f), ), args=(E(Name(alpha), ), E(Template(), ))), ) |}]

let%expect_test _ =
  printAst "[[lam [(x)] {}] {} ]" ;
  [%expect
    {| E(LamApplication(lam=E(Lam(params=[E(Var(name=x), )], lbody=E(Template(), )), ), args=(E(Template(), ))), ) |}]

let%expect_test _ =
  handleParseFail (fun () -> printAst "[[lam [(x alpha [lam [(x)] x])] {}] {} ]") ;
  [%expect
    {| E(LamApplication(lam=E(Lam(params=[E(Var(name=x, E(Lam(params=[E(Var(name=x), )], lbody=E(Name(x), )), ), E(Name(alpha), )), )], lbody=E(Template(), )), ), args=(E(Template(), ))), ) |}]
