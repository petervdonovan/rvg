let rec takeWhileRec pred acc s =
  match Seq.uncons s with
  | Some (c, s') -> if pred c
    then takeWhileRec pred (c :: acc) s'
    else acc, (Seq.cons c s')
  | None -> acc, s
let takeWhile pred s = takeWhileRec pred [] s

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
let charSeq2String s =
  let b = Buffer.create 16 in
    Seq.iter (Buffer.add_char b) s;
    Buffer.contents b
let get2Tokens s =
  match parseToken s with
  | Some (a, s') -> (match parseToken s' with
    | Some (b, s'') -> Some (a, b, s'') | _ -> None) | _ -> None
let get3Tokens s =
  match get2Tokens s with
  | Some (a, b, s') -> (match parseToken s' with
      | Some (c, s'') -> Some (a, b, c, s'') | _ -> None) | _ -> None
let get3TokensWithThirdTokenInParens s =
  match get3Tokens s with
  | Some (reg, imm, openParen, s') -> (
    if openParen <> "(" then None
    else match get2Tokens s' with
    | Some (rs2, closeParen, _) ->
      if closeParen <> ")" then None
      else Some (reg, imm, rs2)
    | None -> None)
  | None -> None
let funNotation name args = name ^ "(" ^ (String.concat ", " args) ^ ")"
