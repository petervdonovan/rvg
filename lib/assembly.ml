exception AsmParseFail of string

open ParseUtil

type register =
  | TempReg of string
  | SaveReg of string
  | Zero
  | Ra
  | Sp
  | Gp
  | Tp
type opcode = string
type immediate = string
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
type t = { top: string; middle: instruction list; bottom: string }

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

let rec nameToReg env name =
  if NameSet.mem name temporaries then TempReg name
  else if NameSet.mem name saved then SaveReg name
  else if name = "zero" then Zero
  else if name = "ra" then Ra
  else if name = "sp" then Sp
  else if name = "gp" then Gp
  else if name = "tp" then Tp
  else nameToReg env (env name "Register name")
let resolveNumericalImm env imm =
  match imm |> int_of_string_opt with
  | Some _ -> imm
  | None -> env imm "Numerical immediate"
let asmParseFail formatDescription s =
  raise (AsmParseFail ("Instruction \"" ^ (charSeq2String s) ^ "\" does not follow the instruction syntax for " ^ formatDescription))
let parseR env opc s =
  match get3Tokens s with
  | Some (rd, rs1, rs2, _) -> RType { opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2 }
  | None -> asmParseFail "R type syntax" s
let parseI env opc s =
    match get3Tokens s with
    | Some (rd, rs1, imm, _) -> IArith {
      opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
      imm = resolveNumericalImm env imm
    }
    | None -> asmParseFail "I type arithmetic instruction syntax" s
let parseLoad env opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rd, imm, rs1) -> Load {
    opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
    imm = resolveNumericalImm env imm
  }
  | None -> asmParseFail "the instruction syntax for stores" s
let parseStore env opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rs2, imm, rs1) -> Store {
    opc = opc; rs2 = nameToReg env rs2; rs1 = nameToReg env rs1;
    imm = resolveNumericalImm env imm
  }
  | None -> asmParseFail "the instruction syntax for loads" s
let parseBranch env opc s =
  match get3Tokens s with
  | Some (rs1, rs2, label, _) -> Branch {
    opc = opc; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2;
    imm = label
  }
  | None -> asmParseFail "the instruction syntax for branches" s
let parseJal env opc s =
  match get2Tokens s with
  | Some (rd, label, _) -> Jal { opc = opc; rd = nameToReg env rd; imm = label }
  | None -> asmParseFail "the instruction syntax for jal" s
let parseJalr env opc s =
  match get3Tokens s with
  | Some (rd, rs1, label, _) -> Jalr {
    opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; imm = label
  }
  | None -> asmParseFail "the instruction syntax for jalr" s
let tryParse env s =
  match parseToken s with
  | Some (opcode, s') ->
    Some (let pred = NameSet.mem opcode in (
      if pred rTypeInstrs then parseR
      else if pred iTypeInstrs then parseI
      else if pred loadInstrs then parseLoad
      else if pred storeInstrs then parseStore
      else if pred branchInstrs then parseBranch
      else if pred jalInstrs then parseJal
      else if pred jalrInstrs then parseJalr
      else raise (AsmParseFail ("Unrecognized opcode " ^ opcode))
    ) env opcode s')
  | None -> None
let tryReduceTop env asm =
  let { top; middle; bottom } = asm in
  match tryParse env (String.to_seq top) with
  | Some instr -> { top = ""; middle = instr :: middle; bottom = bottom }
  | None -> asm
let tryReduceBottom env asm =
  let { top; middle; bottom } = asm in
  match tryParse env (String.to_seq bottom) with
  | Some instr -> { top = top; middle = middle @ [instr]; bottom = "" }
  | None -> asm
let empty = { top = ""; middle = []; bottom = "" }
let append env asm char = let { top; middle; bottom } = asm in
  if List.length middle = 0
  then if char = '\n' then tryReduceTop env asm
    else { top = top ^ (String.make 1 char); middle = middle; bottom = bottom }
  else if char = '\n' then tryReduceBottom env asm
    else { top = top; middle = middle; bottom = bottom ^ (String.make 1 char) }
let parse acc env asm = String.to_seq asm |> Seq.fold_left (append env) acc
let regToString reg = match reg with
  | TempReg name -> "temp-" ^ name
  | SaveReg name -> "save-" ^ name
  | Zero -> "Zero"
  | Ra -> "Ra"
  | Sp -> "Sp"
  | Gp -> "Gp"
  | Tp -> "Tp"
let instrToString instr =
  match instr with
  | RType { opc: opcode; rd: register; rs1: register; rs2: register } ->
    funNotation "RType" ([opc] @ List.map regToString [rd;rs1;rs2])
  | IArith { opc: opcode; rd: register; rs1: register; imm: immediate } ->
    funNotation "IArith" ([opc] @ List.map regToString [rd;rs1] @ [imm])
  | Load { opc: opcode; rd: register; rs1: register; imm: immediate } ->
    funNotation "Load" ([opc] @ List.map regToString [rd;rs1] @ [imm])
  | Store { opc: opcode; rs1: register; rs2: register; imm: immediate } ->
    funNotation "Store" ([opc] @ List.map regToString [rs1;rs2] @ [imm])
  | Branch { opc: opcode; rs1: register; rs2: register; imm: immediate } ->
    funNotation "Branch" ([opc] @ List.map regToString [rs1;rs2] @ [imm])
  | Jal { opc: opcode; rd: register; imm: immediate } ->
    funNotation "Jal" ([opc] @ List.map regToString [rd] @ [imm])
  | Jalr { opc: opcode; rd: register; rs1: register; imm: immediate } ->
    funNotation "Jalr" ([opc] @ List.map regToString [rd;rs1] @ [imm])
let asmToString asm =
  let { top; middle; bottom} = asm in
  top ^ (middle |> List.map instrToString |> String.concat "\n") ^ bottom
