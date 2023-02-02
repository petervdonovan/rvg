open ParseUtil

exception StreamException of string

type position = {
  zeroBasedLine: int;
  zeroBasedCol: int;
  file: string
}
type range = {
  startInclusive: position;
  endExclusive: position;
}
let posToString p = "line " ^ (string_of_int p.zeroBasedLine) ^ ", col " ^ (string_of_int p.zeroBasedCol)
let rangeToString r = posToString r.startInclusive ^ " to " ^ (posToString r.endExclusive)
let origin file = {zeroBasedLine = 0; zeroBasedCol = 0; file}
let incrementedCol p =
  {zeroBasedLine = p.zeroBasedLine; zeroBasedCol = p.zeroBasedCol+1; file = p.file}
let incrementedLine p =
  {zeroBasedLine = p.zeroBasedLine + 1; zeroBasedCol = 0; file = p.file}
let singleCharRange p =
  {startInclusive=p; endExclusive=incrementedCol p}
let lessThan p' p = (p.zeroBasedLine < p'.zeroBasedLine) || (p.zeroBasedLine = p'.zeroBasedLine && p.zeroBasedCol < p'.zeroBasedCol)
let contains p r =
  lessThan (incrementedCol p) r.startInclusive && lessThan r.endExclusive p
type t = {s: char Seq.t; current: position; previous: position Option.t; file: string}

let unconsp spp =
  let {s;current;file;_} = spp in
  match Seq.uncons s with
  | Some (c, s') -> Some (c, {
    s=s';
    current=(if c = '\n' then {
        zeroBasedLine = current.zeroBasedLine + 1;
        zeroBasedCol = 0;
        file
      } else {
        zeroBasedLine = current.zeroBasedLine;
        zeroBasedCol = current.zeroBasedCol + 1;
        file
      });
    previous=Some current;
    file
  }, current)
  | None -> None
let uncons spp = match unconsp spp with
  | Some (c, spp', _) -> Some (c, spp')
  | None -> None
let cons c (sp: t) =
  let {s;current = _; previous; file} = sp in
  match previous with
  | Some pos -> let s' = Seq.cons c s in {s=s'; current=pos; previous=None; file}
  | None -> raise (StreamException "Multiple consecutive cons to stream")
let iter f spp = let {s; _} = spp in Seq.iter f s

let rec takeWhileRec pred acc r s =
  match unconsp s with
  | Some (c, s', p) -> if pred c
    then takeWhileRec pred (c :: acc) {startInclusive=r.startInclusive; endExclusive=s'.current} s'
    else acc, {startInclusive=p; endExclusive=p}, (cons c s')
  | None -> acc, r, s
let takeWhile pred s = takeWhileRec pred [] (singleCharRange s.current) s

let rec consumeWhitespace (stream: t) =
  let csp = unconsp stream in
    match csp with
    | Some (c, s, p) -> if isWhitespace c then consumeWhitespace s else Some (c, s, p)
    | None -> None
let rec parseTokenRec stream startInclusive current: (string * t * range) option =
  let csp = unconsp stream in
    match csp with
    | Some (c, s, p) ->
      if isWhitespace c
        then Some (current, s, {startInclusive;endExclusive = p})
      else if List.mem c ['[';']';'(';')';'{';'}']
        then if current = ""
          then Some (String.make 1 c, s, {startInclusive; endExclusive = p})
          else Some (current, cons c s, {startInclusive; endExclusive = incrementedCol p})
      else parseTokenRec s startInclusive (current ^ String.make 1 c)
    | None -> if current = "" then None else
      let {previous; _} = stream in match previous with
      | Some p -> let endExclusive = incrementedCol p in
        Some (current, stream, {
          startInclusive;
          endExclusive
        })
      | None -> None
let parseToken stream =
  match consumeWhitespace stream with
    | Some (c, s, p) -> parseTokenRec (cons c s) p ""
    | None -> None

let fromFile f = let ic = open_in f in {
  s = (Seq.of_dispenser (fun () -> (
  try Some (input_char ic) with
    End_of_file -> None
  )));
  current=origin f;
  previous=None;
  file=f
}, ic
let fromString p s = {s = String.to_seq s; current = p; previous = None; file = ""}
let charSeqToString s =
  let b = Buffer.create 16 in
    iter (Buffer.add_char b) s;
    Buffer.contents b
let get2Tokens s =
  match parseToken s with
  | Some (a, s', r) -> (match parseToken s' with
    | Some (b, s'', r') -> Some ((a, r), (b, r'), s'') | _ -> None) | _ -> None
let get3Tokens s =
  match get2Tokens s with
  | Some (ar, br, s') -> (match parseToken s' with
      | Some (c, s'', r) -> Some (ar, br, (c, r), s'') | _ -> None) | _ -> None
let get3TokensWithThirdTokenInParens s =
  match get3Tokens s with
  | Some (regR, immR, (openParen, _), s') -> (
    if openParen <> "(" then None
    else match get2Tokens s' with
    | Some (rs2, (closeParen, _), _) ->
      if closeParen <> ")" then None
      else Some (regR, immR, rs2)
    | None -> None)
  | None -> None
