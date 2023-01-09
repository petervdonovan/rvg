exception AsmParseFail of string

type register =
  | TempReg of string
  | SaveReg of string
  | Zero
  | Ra
  | Sp
  | Gp
  | Tp
  | UnboundName of string
type opcode = string (* TODO: Allow to be UnboundName? *)
type immediate =
  | Imm of string
  | UnboundName of string
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
let rTypeInstrs: NameSet.t = strsToNameset ["add"; "sub"; "and"; "or"; "xor"; "sll"; "srl"; "sra"; "slt"; "sltu"; "addi"; "andi"; "ori"; "xori"; "slli"; "srli"; "srai"; "slti"; "sltiu"]
let iTypeInstrs = strsToNameset ["addi"; "ori"; "xori"; "slli"; "srli"; "srai"; "slti"; "sltiu"]
let loadInstrs = strsToNameset ["lb"; "lbu"; "lh"; "lhu"; "lw"]
let storeInstrs = strsToNameset ["sb"; "sh"; "sw"]
let branchInstrs = strsToNameset ["beq"; "bge"; "bgeu"; "blt"; "bltu"; "bne"]
let jalInstrs = strsToNameset ["jal"]
let jalrInstrs = strsToNameset ["jalr"]
let temporaries = strsToNameset ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"]
let saved = strsToNameset ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"]
let nameToReg name =
  if NameSet.mem name temporaries then TempReg name
  else if NameSet.mem name saved then SaveReg name
  else if name = "zero" then Zero
  else if name = "ra" then Ra
  else if name = "sp" then Sp
  else if name = "gp" then Gp
  else if name = "tp" then Tp
  else UnboundName name
let charSeq2String s =
  let b = Buffer.create 16 in
    Seq.iter (Buffer.add_char b) s;
    Buffer.contents b
let get2Tokens s =
  match Ast.parseToken s with
  | Some (a, s') -> (match Ast.parseToken s' with
    | Some (b, s'') -> Some (a, b, s'') | _ -> None) | _ -> None
let get3Tokens s =
  match get2Tokens s with
  | Some (a, b, s') -> (match Ast.parseToken s' with
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
let asmParseFail formatDescription s =
  raise (AsmParseFail ("Instruction \"" ^ (charSeq2String s) ^ "\" does not follow the instruction syntax for " ^ formatDescription))
let tryParseR opc s =
  match get3Tokens s with
  | Some (rd, rs1, rs2, _) -> RType { opc = opc; rd = nameToReg rd; rs1 = nameToReg rs1; rs2 = nameToReg rs2 }
  | None -> asmParseFail "R type syntax" s
let tryParseI opc s =
    match get3Tokens s with
    | Some (rd, rs1, imm, _) -> IArith {
      opc = opc; rd = nameToReg rd; rs1 = nameToReg rs1;
      imm = match int_of_string_opt imm with
      | Some _ -> Imm imm
      | None -> UnboundName imm
    }
    | None -> asmParseFail "I type arithmetic instruction syntax" s
let immOrUnboundName imm =
  match int_of_string_opt imm with
    | Some _ -> Imm imm
    | None -> UnboundName imm
let tryParseLoad opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rd, imm, rs1) -> Load {
    opc = opc; rd = nameToReg rd; rs1 = nameToReg rs1;
    imm = immOrUnboundName imm
  }
  | None -> asmParseFail "the instruction syntax for stores" s
let tryParseStore opc s =
  match get3TokensWithThirdTokenInParens s with
  | Some (rs2, imm, rs1) -> Store {
    opc = opc; rs2 = nameToReg rs2; rs1 = nameToReg rs1;
    imm = immOrUnboundName imm
  }
  | None -> asmParseFail "the instruction syntax for loads" s
let tryParseBranch opc s =
  match get3Tokens s with
  | Some (rs1, rs2, label, _) -> Branch {
    opc = opc; rs1 = nameToReg rs1; rs2 = nameToReg rs2;
    imm = UnboundName label
  }
  | None -> asmParseFail "the instruction syntax for branches" s
let tryParseJal opc s =
  match get2Tokens s with
  | Some (rd, label, _) -> Jal { opc = opc; rd = nameToReg rd; imm = UnboundName label }
  | None -> asmParseFail "the instruction syntax for jal" s
let tryParseJalr opc s =
  match get3Tokens s with
  | Some (rd, rs1, label, _) -> Jalr {
    opc = opc; rd = nameToReg rd; rs1 = nameToReg rs1; imm = UnboundName label
  }
  | None -> asmParseFail "the instruction syntax for jalr" s
let tryParse s =
  match Ast.parseToken s with
  | Some (opcode, s') ->
    Some (let pred = NameSet.mem opcode in (
      if pred rTypeInstrs then tryParseR
      else if pred iTypeInstrs then tryParseI
      else if pred loadInstrs then tryParseLoad
      else if pred storeInstrs then tryParseStore
      else if pred branchInstrs then tryParseBranch
      else if pred jalInstrs then tryParseJal
      else tryParseJalr
    ) opcode s')
  | None -> None
let tryReduceTop asm =
  let { top; middle; bottom } = asm in
  match tryParse (String.to_seq top) with
  | Some instr -> { top = ""; middle = instr :: middle; bottom = bottom }
  | None -> asm
let tryReduceBottom asm =
  let { top; middle; bottom } = asm in
  match tryParse (String.to_seq top) with
  | Some instr -> { top = ""; middle = middle @ [instr]; bottom = bottom }
  | None -> asm
let empty = { top = ""; middle = []; bottom = "" }
let append asm char = let { top; middle; bottom } = asm in
  if List.length middle = 0
  then if char = '\n' then tryReduceTop asm
    else { top = top; middle = middle; bottom = bottom ^ (String.make 1 char) }
  else if char = '\n' then tryReduceBottom asm
    else { top = top ^ (String.make 1 char); middle = middle; bottom = bottom }
