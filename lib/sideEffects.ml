type mode =
  | Tokens
  | Definition of CharStream.position
  | Hover of CharStream.position
  | Execution
let getPosition argvStart =
  let zeroBasedLine = int_of_string (Array.get Sys.argv (argvStart + 0)) in
  let zeroBasedCol = int_of_string (Array.get Sys.argv (argvStart + 1)) in
  let file = Array.get Sys.argv (argvStart + 2) in
  ({ zeroBasedLine; zeroBasedCol; file}: CharStream.position)
let currentMode = match Array.get Sys.argv 1 with
  | "tokens" -> Tokens
  | "definition" -> Definition (getPosition 2)
  | "hover" -> Hover (getPosition 2)
  | _ -> Execution
let nargs = match currentMode with
  | Tokens -> 1
  | Definition _ -> 4
  | Hover _ -> 4
  | Execution -> 0
let sideEffectsAllowed = currentMode = Execution
let rec unconditionalPrint arg = match arg with
  | Ast.ParsedAsm pasm, _ -> Assembly.print pasm
  | Ast.Template tem, _ -> List.iter (fun x -> unconditionalPrint x |> ignore;) tem;
  | Ast.Asm asm, _ -> print_string asm;
  | Ast.Integer i, _ -> print_int i;
  | _ -> print_string (Ast.exprToString arg)
let print arg =
  if sideEffectsAllowed then unconditionalPrint arg; arg
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
exception Unreachable
let reportResolvedName range expr =
  let content, ({ r=definitionRange; attrs }: Ast.metadata) = expr in
  match currentMode with
  | Tokens -> announceToken expr range
  | Definition p when CharStream.contains p range ->
    print_endline ("{\"range\": " ^ (definitionRange |> rangeToJson) ^ "}")
  | Hover p when CharStream.contains p range ->
    print_endline ({|{"attrs": [|} ^ (
      attrs |> Ast.Attributes.to_seq |> Seq.map (fun s -> "\"" ^ s ^ "\"") |> List.of_seq |> String.concat ", "
      ) ^ {|], "typeUnionOf": ["|} ^ (
        match content with
        | Ast.Asm _ -> raise Unreachable
        | Ast.Def _ -> raise Unreachable
        | Ast.Lam _ -> "lam"
        | Ast.LamApplication _ -> raise Unreachable
        | Ast.Name _ -> raise Unreachable
        | Ast.ParsedAsm a -> (match a with
          | Assembly.Block _ -> raise Unreachable
          | Assembly.FinishedBlock _ -> "block"
          | Assembly.Fragment _ -> "fragment")
        | Ast.Template _ -> raise Unreachable
        | Ast.Var _ -> raise Unreachable
        | Ast.Integer _ -> raise Unreachable
      ) ^ {|"], "cycles": |} ^ (match content with
        | Ast.ParsedAsm (Assembly.FinishedBlock (
            {totalCycles=Some k; _}: Assembly.finished_block)
          ) -> string_of_int k
        | _ -> "null") ^ {|, "cyclesMod": [|} ^ (match content with
        | Ast.ParsedAsm (Assembly.FinishedBlock (
            {cyclesMod; _}: Assembly.finished_block)
          ) -> Assembly.CyclesModMap.bindings cyclesMod |> List.map (fun (m, x) -> (string_of_int x) ^ " mod " ^ (string_of_int m)) |> String.concat ", "
        | _ -> "") ^ {|], "range": |} ^ (
          rangeToJson range
        ) ^ {|}|})
  | _ -> ()
