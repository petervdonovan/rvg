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
  | Label of string
type fragment = string
type finished_block =
  | MetaBlock of finished_block list
  | Instruction of instruction
type block = { top: fragment; middle: finished_block list; bottom: fragment }
type t =
  | Fragment of fragment
  | Block of block
  | FinishedBlock of finished_block
let isEmpty fragment = fragment = ""
let empty = Fragment ""

module NameSet = Set.Make(String)
let strsToNameset strs = List.fold_right NameSet.add strs NameSet.empty
let rTypeInstrs: NameSet.t = strsToNameset ["add"; "sub"; "and"; "or"; "xor"; "sll"; "srl"; "sra"; "slt"; "sltu"]
let iTypeInstrs = strsToNameset ["addi"; "ori"; "xori"; "slli"; "srli"; "srai"; "slti"; "sltiu"]
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
  | Label s -> "Label(" ^ s ^ ":" ^ ")"
let rec finishedBlockToString fb =
  match fb with
  | Instruction i -> instrToString i
  | MetaBlock mb -> funNotation "MetaBlock" [
    mb |> List.map finishedBlockToString |> String.concat "\n"]
let rec asmToString asm =
  match asm with
  | Fragment s -> funNotation "Fragment" [s]
  | Block {top; middle; bottom} -> funNotation "Block"
    [top; asmToString (FinishedBlock (MetaBlock middle)); bottom]
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
let asmParseFail formatDescription s =
  raise (AsmParseFail ("Instruction \"" ^ (charSeqToString s) ^ "\" does not follow the instruction syntax for " ^ formatDescription))
let parseR env opc s =
  match get3Tokens s with
  | Some (rd, rs1, rs2, _) -> Some (RType { opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2 })
  | None -> None
let parseI env opc s =
    match get3Tokens s with
    | Some (rd, rs1, imm, _) -> Some (IArith {
      opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
      imm = resolveNumericalImm env imm
    })
    | None -> None
let parseLoad env opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rd, imm, rs1) -> Some (Load {
    opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
    imm = resolveNumericalImm env imm
  })
  | None -> None
let parseStore env opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rs2, imm, rs1) -> Some (Store {
    opc = opc; rs2 = nameToReg env rs2; rs1 = nameToReg env rs1;
    imm = resolveNumericalImm env imm
  })
  | None -> None
let parseBranch env opc s =
  match get3Tokens s with
  | Some (rs1, rs2, label, _) -> Some (Branch {
    opc = opc; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2;
    imm = label
  })
  | None -> None
let parseJal env opc s =
  match get2Tokens s with
  | Some (rd, label, _) -> Some (Jal { opc = opc; rd = nameToReg env rd; imm = label })
  | None -> None
let parseJalr env opc s =
  match get3Tokens s with
  | Some (rd, rs1, label, _) -> Some (Jalr {
    opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; imm = label
  })
  | None -> None
let tryParse env str =
  let s = CharStream.fromString origin str in
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
      else if String.ends_with ~suffix:":" opcode
        then fun _ _ _ -> Some (Label (
          String.sub opcode 0 (String.length opcode - 1)))
        else fun _ _ _ -> None
    ) env (opcode, r) s')
  | None -> None
let append env asm char =
  match asm with
  | Fragment s ->
    (match if char = '\n' then tryParse env s else None with
    | Some instr -> Block {top = ""; middle = [Instruction instr]; bottom = ""}
    | _ -> Fragment (s ^ String.make 1 char))
  | Block {top;middle;bottom} ->
    (match if char = '\n' then tryParse env bottom else None with
    | Some instr when char = '\n' -> Block {
        top;
        middle = middle @ [Instruction instr];
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
        middle = middle @ [Instruction parsed] @ acc.middle;
        bottom = acc.bottom
      }
    | None -> if String.trim glue = "" then {top; middle = middle @ acc.middle; bottom = acc.bottom} else raise glueFail)
  | FinishedBlock fb ->
    let gluableAcc = if String.trim acc.top = "" then acc else (
      match tryParse env acc.top with
      | Some inst -> {top=""; middle = [Instruction inst] @ acc.middle; bottom = acc.bottom}
      | None -> raise glueFail
    ) in
    {top = ""; middle = [fb] @ gluableAcc.middle; bottom = gluableAcc.bottom}
(* Blocks must have a middle; else they are fragments;
   they must have sticky ends; else they are finished *)
let rec promoteOrDemote env b =
    if List.length b.middle = 0 then Fragment b.top
    else if b.top = ""
      then if b.bottom = ""
        then FinishedBlock (MetaBlock b.middle)
      else match tryParse env b.bottom with
      | Some instr -> promoteOrDemote env
        {top = ""; middle = b.middle @ [Instruction instr]; bottom = ""}
      | None -> Block b
    else match tryParse env b.top with
      | Some instr -> promoteOrDemote env
        {top = ""; middle = b.middle @ [Instruction instr]; bottom = ""}
    | None -> Block b
let parse acc env asm = String.to_seq asm
  |> Seq.fold_left (append env) acc
  |> fun a -> (
    match a with
    | Block b -> promoteOrDemote env b
    | Fragment f -> (match tryParse env f with
      | Some instr -> FinishedBlock (Instruction instr)
      | None -> a)
    | FinishedBlock _ -> a
  )
let rec print pasm =
  match pasm with
  | Fragment f -> print_string f
  | Block b ->
    b.top |> String.trim |> print_string;
    List.iter printFinishedBlock b.middle;
    b.bottom |> String.trim |> print_string;
  | FinishedBlock fb -> printFinishedBlock fb
and printFinishedBlock fb =
  match fb with
  | Instruction i -> print_string (stringifyInstruction i)
  | MetaBlock mb -> List.iter printFinishedBlock mb
and stringifyInstruction i =
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
  let (=) s imm = s ^ " " ^ (fst imm) in
  (match i with
  | RType { opc; rd; rs1; rs2 }   -> opc < rd $ rs1 $ rs2
  | IArith { opc; rd; rs1; imm }  -> opc < rd $ rs1 = imm
  | Load { opc; rd; rs1; imm }    -> opc < rd = imm $$ rs1
  | Store { opc; rs1; rs2; imm }  -> opc < rs2 = imm $$ rs1
  | Branch { opc; rs1; rs2; imm } -> opc < rs1 $ rs2 = imm
  | Jal { opc; rd; imm }          -> opc < rd = imm
  | Jalr { opc; rd; rs1; imm }    -> opc < rd $ rs1 = imm
  | Label _ -> instrToString i) ^ "\n"
