exception AsmParseFail of string

open ParseUtil
open CharStream

type register =
  | TempReg of string * range
  | SaveReg of string * range
  | Zero of range
  | Ra of range
  | Sp of range
  | Gp of range
  | Tp of range
type opcode = string * range
type immediate = string * range
type r_format = { opc: opcode; rd: register; rs1: register; rs2: register }
type i_format = { opc: opcode; rd: register; rs1: register; imm: immediate }
type s_or_b_format = { opc: opcode; rs1: register; rs2: register; imm: immediate }
type u_or_j_format = { opc: opcode; rd: register; imm: immediate }
type instruction =
  | RType of r_format
  | IArith of i_format
  | Load of i_format
  | Store of s_or_b_format
  | Branch of s_or_b_format
  | Jal of u_or_j_format
  | Jalr of i_format
  | UType of u_or_j_format
  | Label of string * bool
type fragment = string
type nonce = string
module Noncification = Map.Make(String)
module CyclesModMap = Map.Make(Int)
type finished_block_content =
  | MetaBlock of finished_block list
  | Instruction of instruction
and finished_block = {
  content: finished_block_content;
  provides: nonce Noncification.t;
  totalCycles: int option;
  cyclesMod: int CyclesModMap.t
}
type block = { top: fragment; middle: finished_block list; bottom: fragment }
type t =
  | Fragment of fragment
  | Block of block
  | FinishedBlock of finished_block
let isEmpty fragment = fragment = ""
let empty = Fragment ""
let finishedBlockOf content = {
  content;
  provides = (match content with
  | MetaBlock mb -> List.fold_left
    (Noncification.union (fun _ a _ -> Some a))
    Noncification.empty
    (List.map (fun c -> c.provides) mb)
  | Instruction instr -> (match instr with
    | Label (s, noncify) -> Noncification.singleton s (if noncify then "_" ^ nonce () else "")
    | _ -> Noncification.empty
  ));
  totalCycles = None;
  cyclesMod = CyclesModMap.empty
}

module NameSet = Set.Make(String)
let strsToNameset strs = List.fold_right NameSet.add strs NameSet.empty
let rTypeInstrs: NameSet.t = strsToNameset ["add"; "sub"; "and"; "or"; "xor"; "sll"; "srl"; "sra"; "slt"; "sltu"]
let iTypeInstrs = strsToNameset ["addi"; "ori"; "xori"; "slli"; "srli"; "srai"; "slti"; "sltiu"; "csrrw"; "csrrs"; "csrrc"; "csrrwi"; "csrrsi"; "csrrci"]
let loadInstrs = strsToNameset ["lb"; "lbu"; "lh"; "lhu"; "lw"]
let storeInstrs = strsToNameset ["sb"; "sh"; "sw"]
let branchInstrs = strsToNameset ["beq"; "bge"; "bgeu"; "blt"; "bltu"; "bne"]
let jalInstrs = strsToNameset ["jal"]
let jalrInstrs = strsToNameset ["jalr"]
let temporaries = strsToNameset ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"]
let saved = strsToNameset ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"]
let failWithNoBinding name = raise (AsmParseFail ("Could not find binding for name \"" ^ name ^ "\""))
let regToString reg = match reg with
  | TempReg (name, _) -> "temp-" ^ name
  | SaveReg (name, _) -> "save-" ^ name
  | Zero _ -> "Zero"
  | Ra _ -> "Ra"
  | Sp _ -> "Sp"
  | Gp _ -> "Gp"
  | Tp _ -> "Tp"
let instrToString instr =
  match instr with
  | RType { opc: opcode; rd: register; rs1: register; rs2: register } ->
    funNotation "RType" ([fst opc] @ List.map regToString [rd;rs1;rs2])
  | IArith { opc: opcode; rd: register; rs1: register; imm: immediate } ->
    funNotation "IArith" ([fst opc] @ List.map regToString [rd;rs1] @ [fst imm])
  | Load { opc: opcode; rd: register; rs1: register; imm: immediate } ->
    funNotation "Load" ([fst opc] @ List.map regToString [rd;rs1] @ [fst imm])
  | Store { opc: opcode; rs1: register; rs2: register; imm: immediate } ->
    funNotation "Store" ([fst opc] @ List.map regToString [rs1;rs2] @ [fst imm])
  | Branch { opc: opcode; rs1: register; rs2: register; imm: immediate } ->
    funNotation "Branch" ([fst opc] @ List.map regToString [rs1;rs2] @ [fst imm])
  | Jal { opc: opcode; rd: register; imm: immediate } ->
    funNotation "Jal" ([fst opc] @ List.map regToString [rd] @ [fst imm])
  | Jalr { opc: opcode; rd: register; rs1: register; imm: immediate } ->
    funNotation "Jalr" ([fst opc] @ List.map regToString [rd;rs1] @ [fst imm])
  | UType { opc: opcode; rd: register; imm: immediate } ->
    funNotation "UType" ([fst opc] @ [regToString rd] @ [fst imm] )
  | Label (s, noncify) -> "Label(" ^ s ^ " " ^ (string_of_bool noncify) ^ ")"
let rec finishedBlockToString fb =
  match fb.content with
  | Instruction i -> instrToString i
  | MetaBlock mb -> funNotation "MetaBlock" [
    mb |> List.map finishedBlockToString |> String.concat "\n"]
let rec asmToString asm =
  match asm with
  | Fragment s -> funNotation "Fragment" [s]
  | Block {top; middle; bottom} -> funNotation "Block"
    [top; asmToString (FinishedBlock (finishedBlockOf (MetaBlock middle))); bottom]
  | FinishedBlock fb -> finishedBlockToString fb
let rec nameToReg env name =
  let str, r = name in
  if NameSet.mem str temporaries then TempReg (str, r)
  else if NameSet.mem str saved then SaveReg (str, r)
  else if str = "zero" then Zero r
  else if str = "ra" then Ra r
  else if str = "sp" then Sp r
  else if str = "gp" then Gp r
  else if str = "tp" then Tp r
  else nameToReg env (env str "Register name")
let resolveNumericalImm env imm =
  let s, _ = imm in
  match s |> int_of_string_opt with
  | Some _ -> imm
  | None -> env s "Numerical immediate"
let resolveLabel env (label: string * range) =
  let l, _ = label in
  try env l "Label" with AsmParseFail _ -> label
let asmParseFail formatDescription s =
  raise (AsmParseFail ("Instruction \"" ^ (charSeqToString s) ^ "\" does not follow the instruction syntax for " ^ formatDescription))
let parseR env opc s =
  match get3Tokens s with
  | Some (rd, rs1, rs2, _) -> Some (Instruction(RType { opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2 }))
  | None -> None
let parseI env opc s =
    match get3Tokens s with
    | Some (rd, rs1, imm, _) -> Some (Instruction(IArith {
      opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
      imm = resolveNumericalImm env imm
    }))
    | None -> None
let parseLoad env opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rd, imm, rs1) -> Some (Instruction(Load {
    opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
    imm = resolveNumericalImm env imm
  }))
  | None -> None
let parseStore env opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rs2, imm, rs1) -> Some (Instruction(Store {
    opc = opc; rs2 = nameToReg env rs2; rs1 = nameToReg env rs1;
    imm = resolveNumericalImm env imm
  }))
  | None -> None
let parseBranch env opc s =
  match get3Tokens s with
  | Some (rs1, rs2, label, _) -> Some (Instruction(Branch {
    opc = opc; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2;
    imm = resolveLabel env label
  }))
  | None -> None
let parseJal env opc s =
  match get2Tokens s with
  | Some (rd, label, _) -> Some (Instruction(Jal { opc = opc; rd = nameToReg env rd; imm = resolveLabel env label }))
  | None -> None
let parseJalr env opc s =
  match get3Tokens s with
  | Some (rd, rs1, label, _) -> Some (Instruction(Jalr {
    opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; imm = resolveLabel env label
  }))
  | None -> None
let parseU env opc s =
  match get2Tokens s with
  | Some (rd, imm, _) -> Some (Instruction(UType { opc = opc; rd = nameToReg env rd; imm }))
  | None -> None
let parseBxxz env opc s = (
  let opcs, r = opc in
  match get2Tokens s with
  | Some (rs1, label, _) -> Some (Instruction(Branch {
      opc = if opcs = "beqz" then "beq", r else "bnez", r;
      rs1 = nameToReg env rs1;
      rs2=Zero r;
      imm=resolveLabel env label
    }))
  | None -> None)
let parseJ env opc s =
  let _, r = opc in
  match CharStream.parseToken s with
  | Some (label, _, r') -> Some (Instruction(Jal {
    opc = "jal", r;
    rd = Zero r;
    imm = resolveLabel env (label, r')
  }))
  | None -> None
let parseJr env opc s =
  let _, r = opc in
  match CharStream.parseToken s with
  | Some (rs1s, _, r') -> Some (Instruction(Jalr {
    opc = "jalr", r;
    rd = Zero r;
    rs1 = nameToReg env (rs1s, r');
    imm = "0", r
  }))
  | None -> None
let parseLa _ _ _ = raise (AsmParseFail "la is not currently supported")
let parseLi env opc s =
  let _, r = opc in
  match get2Tokens s with
  | Some (rd, (imm, immr), _) -> (
    let ks, _ = resolveNumericalImm env (imm, immr) in
    let k = int_of_string ks in
    let upper = k / 4096 in
    let lower = k - 4096 * upper in
    let lower = if lower > 2047 then lower - 4096 else lower in
    let upper = (k - lower) / 4096 in
    let rd = nameToReg env rd in
    Some (MetaBlock (
      (if upper = 0 then [] else [finishedBlockOf (Instruction(UType {opc="lui", r; rd; imm=string_of_int upper, immr}))])
      @ (if lower = 0 && upper <> 0 then [] else [finishedBlockOf (Instruction(IArith {opc="addi", r; rd; rs1 = (if upper = 0 then Zero r else rd); imm=string_of_int lower, immr}))]))))
  | None -> None
let parseMv env opc s =
  let _, r = opc in
  match get2Tokens s with
  | None -> None
  | Some (rd, rs1, _) -> Some (Instruction(IArith({
    opc="addi", r; rd=nameToReg env rd; rs1 = nameToReg env rs1; imm="0", r
  })))
let parseNeg env opc s =
  let _, r = opc in
  match get2Tokens s with
  | None -> None
  | Some (rd, rs1, _) -> Some (Instruction(RType({
    opc="sub", r; rd=nameToReg env rd; rs1 = nameToReg env rs1; rs2=Zero r
  })))
let parseNop _ opc _ =
  let _, r = opc in Some(Instruction(IArith({
    opc="addi", r; rd=Zero r; rs1 = Zero r; imm="0", r
  })))
let parseNot env opc s =
  let _, r = opc in
  match get2Tokens s with
  | None -> None
  | Some (rd, rs1, _) -> Some (Instruction(IArith({
    opc="xori", r; rd=nameToReg env rd; rs1 =nameToReg env rs1; imm="-1", r
  })))
let parseRet _ opc _ =
  let _, r = opc in Some(Instruction(Jalr({
    opc="jalr", r; rd=Zero r; rs1=Ra r; imm="0", r
  })))
let parseCsrw env opc s =
  let _, r = opc in
  match get2Tokens s with
  | None -> None
  | Some (csr, source, _) -> Some(Instruction(IArith({
    opc="csrrw", r; rd=Zero r; rs1=nameToReg env source; imm=resolveNumericalImm env csr
  })))
let parseRdcycle env opc s =
  let _, r = opc in
  match parseToken s with
  | None -> None
  | Some (rd, _, r') -> Some(Instruction(IArith({
    opc="csrrs", r'; rd=nameToReg env (rd, r'); rs1=Zero r; imm="0xb00", r
  })))
let tryParse env str =
  let s = CharStream.fromString ("" (*this is wrong*)) str in
  match parseToken s with
  | Some (opcode, s', r) ->
    (let pred = NameSet.mem opcode in (
      if pred rTypeInstrs then parseR
      else if pred iTypeInstrs then parseI
      else if pred loadInstrs then parseLoad
      else if pred storeInstrs then parseStore
      else if pred branchInstrs then parseBranch
      else if pred jalInstrs then parseJal
      else if pred jalrInstrs then parseJalr
      else if pred (strsToNameset ["auipc"; "lui"]) then parseU
      else if opcode = "beqz" || opcode = "bnez" then parseBxxz
      else if opcode = "j" then parseJ
      else if opcode = "jr" then parseJr
      else if opcode = "la" then parseLa
      else if opcode = "li" then parseLi
      else if opcode = "mv" then parseMv
      else if opcode = "neg" then parseNeg
      else if opcode = "nop" then parseNop
      else if opcode = "not" then parseNot
      else if opcode = "ret" then parseRet
      else if opcode = "csrw" then parseCsrw
      else if opcode = "rdcycle" then parseRdcycle
      else if String.ends_with ~suffix:":" opcode
        then fun _ _ _ -> Some (Instruction(Label (
          String.sub opcode 0 (String.length opcode - 1),
          String.capitalize_ascii opcode = opcode
        )))
        else fun _ _ _ -> None
    ) env (opcode, r) s')
  | None -> None
let append env asm char =
  match asm with
  | Fragment s ->
    (match if char = '\n' then tryParse env s else None with
    | Some instr -> Block {top = ""; middle = [instr |> finishedBlockOf]; bottom = ""}
    | _ -> Fragment (s ^ String.make 1 char))
  | Block {top;middle;bottom} ->
    (match if char = '\n' then tryParse env bottom else None with
    | Some instr when char = '\n' -> Block {
        top;
        middle = middle @ [instr |> finishedBlockOf];
        bottom = ""
      }
    | _ -> Block {
      top;
      middle;
      bottom = bottom ^ (String.make 1 char)
    })
  | FinishedBlock fb -> Block {
      top = "";
      middle = [fb];
      bottom = String.make 1 char
    }
let prependBlock env acc prependable =
  let glueFail = (AsmParseFail (
    "\"" ^ (asmToString prependable) ^ "\" and \""
    ^ (asmToString (Block acc))
    ^ " cannot be glued together."
  )) in
  match prependable with
  | Fragment s -> {top = s ^ acc.top; middle = acc.middle; bottom = acc.bottom}
  | Block {top; middle; bottom} -> (
    let glue = bottom ^ acc.top in
    match tryParse env glue with
    | Some parsed -> {
        top;
        middle = middle @ [parsed |> finishedBlockOf] @ acc.middle;
        bottom = acc.bottom
      }
    | None -> if String.trim glue = "" then {top; middle = middle @ acc.middle; bottom = acc.bottom} else raise glueFail)
  | FinishedBlock fb ->
    let gluableAcc = if String.trim acc.top = "" then acc else (
      match tryParse env acc.top with
      | Some inst -> {top=""; middle = [inst |> finishedBlockOf] @ acc.middle; bottom = acc.bottom}
      | None -> raise glueFail
    ) in
    {top = ""; middle = [fb] @ gluableAcc.middle; bottom = gluableAcc.bottom}
(* Blocks must have a middle; else they are fragments;
   they must have sticky ends; else they are finished *)
let rec promoteOrDemote env b =
  if String.trim b.top <> "" then match tryParse env b.top with
    | Some instr -> promoteOrDemote env
      {top = ""; middle = [instr |> finishedBlockOf] @ b.middle; bottom = ""}
    | None -> if List.length b.middle = 0 then Fragment b.top else Block b
  else if String.trim b.bottom = ""
      then if List.length b.middle <> 0 then FinishedBlock (MetaBlock b.middle |> finishedBlockOf) else Fragment (b.top ^ b.bottom)
    else match tryParse env b.bottom with
    | Some inst -> promoteOrDemote env
      {top = ""; middle = b.middle @ [inst |> finishedBlockOf]; bottom = ""}
    | None -> Block b
let parse acc env asm = String.to_seq asm
  |> Seq.fold_left (append env) acc
  |> fun a -> (
    match a with
    | Block b -> promoteOrDemote env b
    | Fragment f -> (match tryParse env f with
      | Some instr -> FinishedBlock (instr |> finishedBlockOf)
      | None -> a)
    | FinishedBlock _ -> a
  )
let rec print pasm =
  match pasm with
  | Fragment f -> print_string f
  | Block b ->
    b.top |> String.trim |> print_string;
    List.iter (printFinishedBlock []) b.middle;
    b.bottom |> String.trim |> print_string;
  | FinishedBlock fb -> printFinishedBlock [] fb
and printFinishedBlock hn fb =
  let hn' = List.cons fb.provides hn in
  match fb.content with
  | Instruction i -> print_string (stringifyInstruction hn' i)
  | MetaBlock mb -> List.iter (printFinishedBlock hn') mb
and stringifyInstruction hierarchicalNoncifications i =
  let noncify label = try
      label ^ Noncification.find label (List.find (Noncification.mem label) hierarchicalNoncifications)
    with Not_found -> print_endline ("Could not find label " ^ label ^ " in the current context, and so could not print the label with the correct nonce"); raise Not_found in
  let ($) s reg = s ^ (match reg with
    | TempReg (s, _) -> s
    | SaveReg (s, _) -> s
    | Zero _ -> "zero"
    | Ra _ -> "ra"
    | Sp _ -> "sp"
    | Gp _ -> "gp"
    | Tp _ -> "tp") in
  let ($$) s (reg: register) = (s ^ "(" $ reg) ^ ")" in
  let (<) opc t = "    " ^ (fst opc) ^ " " $ t in
  let ($) s reg = (s ^ ", ") $ reg in
  let (=) s imm = s ^ ", " ^ (fst imm) in
  let (==) s imm = s ^ ", " ^ (imm |> fst |> noncify) in
  (match i with
  | RType { opc; rd; rs1; rs2 }   -> opc < rd $ rs1 $ rs2
  | IArith { opc; rd; rs1; imm }  -> if String.equal (fst opc) "csrrw" then opc < rd = imm $ rs1 else opc < rd $ rs1 = imm
  | Load { opc; rd; rs1; imm }    -> opc < rd = imm $$ rs1
  | Store { opc; rs1; rs2; imm }  -> opc < rs2 = imm $$ rs1
  | Branch { opc; rs1; rs2; imm } -> opc < rs1 $ rs2 == imm
  | Jal { opc; rd; imm }          -> opc < rd == imm
  | Jalr { opc; rd; rs1; imm }    -> opc < rd $ rs1 == imm
  | UType {opc; rd; imm}          -> opc < rd = imm
  | Label (s, _) -> noncify s ^ ":") ^ "\n"
