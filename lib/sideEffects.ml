type mode =
  | Tokens
  | Definition of CharStream.position
  | Execution
let currentMode = match Array.get Sys.argv 1 with
  | "tokens" -> Tokens
  | "definition" ->
    let zeroBasedLine = int_of_string (Array.get Sys.argv 2) in
    let zeroBasedCol = int_of_string (Array.get Sys.argv 3) in
    let file = Array.get Sys.argv 4 in
    Definition ({ zeroBasedLine; zeroBasedCol; file}: CharStream.position)
  | _ -> Execution
let nargs = match currentMode with
  | Tokens -> 1
  | Definition _ -> 4
  | Execution -> 0
let sideEffectsAllowed = currentMode = Execution
let print arg =
  (match arg with
  | Ast.ParsedAsm pasm, _ -> if sideEffectsAllowed then Assembly.print pasm; arg
  | _ -> if sideEffectsAllowed then print_endline (Ast.exprToString arg); arg)
let posToList (p: CharStream.position) = "[" ^ (string_of_int p.zeroBasedLine) ^ ", " ^ (string_of_int p.zeroBasedCol) ^ "]"
let rangeToList (r: CharStream.range) = "[" ^ (posToList r.startInclusive) ^ ", " ^ (posToList r.endExclusive) ^ "]"
let rangeToJson (r: CharStream.range) = "{ \"file\": \"" ^ r.startInclusive.file ^ "\", \"range\": " ^ (rangeToList r) ^ " }"
let announceToken expr (r: CharStream.range) =
  let content, ({attrs; _}: Ast.metadata) = expr in
  let kind = (match content with
    | Ast.Lam _ -> Some "function"
    | Ast.ParsedAsm _ -> Some "string"
    | _ -> None) in
  let modifier = if Ast.Attributes.mem "std" attrs then "defaultLibrary" else "" in
  match kind with
  | Some kind ->
    if (Array.get Sys.argv 1 = "tokens") then print_endline ("{\"kind\": \"" ^ kind ^ "\", \"modifier\": \"" ^ modifier ^ "\", \"range\": " ^ rangeToJson r ^ " }")
  | None -> ()
let reportResolvedName range definition =
  let _, ({ r=definitionRange; attrs = _ }: Ast.metadata) = definition in
  match currentMode with
  | Tokens -> announceToken definition range
  | Definition p -> if CharStream.contains p range then
    print_endline ("{\"range\": " ^ (definitionRange |> rangeToJson) ^ "}") else ()
  | _ -> ()
