open Ast

let asm2CharStream asm =
  let asm, meta = asm in
  match asm with
  | Asm asm ->
      ( { s= String.to_seq asm
        ; current= meta.r.startInclusive
        ; previous= None
        ; file= meta.r.startInclusive.file }
        : CharStream.t )
  | _ ->
      raise
        (ParseFail
           ("Expected assembly but got " ^ (asm |> exprContentToString), meta.r.startInclusive) )

let charStream2asm (s : CharStream.t) attrs =
  let asm, r, _ = CharStream.takeWhile (fun _ -> true) s in
  if asm = "" then None else Some (Asm asm, {attrs; r})

let rec startsWithWord e =
  let e', _ = e in
  match e' with
  | Template tem ->
      let (exprs, _) = Eval.fullyEvalTem tem in
      if List.length exprs = 0 then true else startsWithWord (List.hd exprs)
  | Asm s ->
      if s = "" then false
      else if ParseUtil.isWhitespace s.[0] || ParseUtil.isSymbol s.[0] then false
      else true
  | _ ->
      false

let rec parseToken e =
  let e', meta = e in
  match e' with
  | Template tem -> (
      let (exprs, env) = Eval.fullyEvalTem tem in
      if List.length exprs = 0 then (None, Some env, None)
      else
        let token, env', remainder = exprs |> List.hd |> parseToken in
        let chosenEnv = Some (Option.value env' ~default:env) in
        let tail = List.tl exprs in
        match token with
        | Some (Asm token, meta') -> (
          match remainder with
          | Some remainder ->
              (Some (Asm token, meta'), chosenEnv, Some (Template (remainder :: tail, env), meta))
          | None -> (
              if List.length tail = 0 then (Some (Asm token, meta'), chosenEnv, None)
              else
                let remainder = (Template (tail, env), meta) in
                if not (startsWithWord remainder) then
                  (Some (Asm token, meta'), chosenEnv, Some remainder)
                else
                  let rest, _, remainder' = parseToken remainder in
                  match rest with
                  | Some (Asm rest, meta'') ->
                      ( Some
                          ( Asm (token ^ rest)
                          , {attrs= Attributes.union meta''.attrs meta'.attrs; r= meta'.r} )
                      , chosenEnv
                      , remainder' )
                  | _ ->
                      (Some (Asm token, meta'), chosenEnv, Some remainder) ) )
        | Some _ -> (
            ( token
            , chosenEnv
            , match remainder with
              | Some remainder ->
                  Some (Template (remainder :: tail, env), meta)
              | None ->
                  Some (Template (tail, env), meta) ) )
        | None ->
            parseToken (Template (List.tl exprs, env), meta) )
  | Asm _ -> (
      let s = asm2CharStream e in
      match CharStream.parseToken s with
      | Some (asm, s, r) ->
          let remainder = charStream2asm s meta.attrs in
          (Some (Asm asm, {attrs= meta.attrs; r}), None, remainder)
      | None ->
          (None, None, None) )
  | _ ->
      (Some e, None, None)

let parseTokenExpectingString env e =
  let token, env', e' = parseToken e in
  match token with
  | Some (Asm s, meta) ->
      (Some ((s, meta.r), Option.value env' ~default:env), e')
  | Some (Integer i, meta) ->
      (Some ((string_of_int i, meta.r), Option.value env' ~default:env), e')
  | None ->
      (None, e')
  | Some (e, meta) ->
      raise
        (Eval.EvalFail
           ( "expected string but got " ^ (e |> Ast.exprContentToString)
           , [(meta.r : CharStream.range)] ) )

let get2Tokens env e =
  match parseTokenExpectingString env e with
  | Some a, Some e' -> (
    match parseTokenExpectingString env e' with
    | Some b, e'' ->
        (Some (a, b), e'')
    | _ ->
        (None, None) )
  | _ ->
      (None, None)

let get3Tokens env e =
  match get2Tokens env e with
  | Some (a, b), Some e' -> (
    match parseTokenExpectingString env e' with
    | Some c, e'' ->
        (Some (a, b, c), e'')
    | _ ->
        (None, None) )
  | _ ->
      (None, None)

let get3TokensWithThirdTokenInParens env e =
  match get3Tokens env e with
  | Some (regR, immR, ((openParen, _), _)), Some e' -> (
      if openParen <> "(" then (None, Some e)
      else
        match get2Tokens env e' with
        | Some (rs2, ((closeParen, _), _)), e'' ->
            if closeParen <> ")" then (None, Some e) else (Some (regR, immR, rs2), e'')
        | _ ->
            (None, Some e) )
  | _ ->
      (None, Some e)
