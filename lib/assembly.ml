type register =
  | TempReg of string * CharStream.range
  | SaveReg of string * CharStream.range
  | Zero of CharStream.range
  | Ra of CharStream.range
  | Sp of CharStream.range
  | Gp of CharStream.range
  | Tp of CharStream.range

type opcode = string * CharStream.range

type immediate = string * CharStream.range

type r_format = {opc: opcode; rd: register; rs1: register; rs2: register}

type i_format = {opc: opcode; rd: register; rs1: register; imm: immediate}

type s_or_b_format = {opc: opcode; rs1: register; rs2: register; imm: immediate}

type u_or_j_format = {opc: opcode; rd: register; imm: immediate}

type instruction =
  | RType of r_format
  | IArith of i_format
  | Csr of i_format
  | Load of i_format
  | Store of s_or_b_format
  | Branch of s_or_b_format
  | Jal of u_or_j_format
  | Jalr of i_format
  | UType of u_or_j_format
  | Label of string * bool

type fragment = string

type nonce = string

module Noncification = Map.Make (String)
module CyclesModMap = Map.Make (Int)

type finished_block_content = MetaBlock of finished_block list | Instruction of instruction

and finished_block =
  { content: finished_block_content
  ; provides: nonce Noncification.t
  ; totalCycles: int option
  ; cyclesMod: int CyclesModMap.t }

type block = {top: fragment; middle: finished_block list; bottom: fragment}

type t = Fragment of fragment | Block of block | FinishedBlock of finished_block

let regToString reg =
  match reg with
  | TempReg (name, _) ->
      "temp-" ^ name
  | SaveReg (name, _) ->
      "save-" ^ name
  | Zero _ ->
      "Zero"
  | Ra _ ->
      "Ra"
  | Sp _ ->
      "Sp"
  | Gp _ ->
      "Gp"
  | Tp _ ->
      "Tp"

let instrToString instr =
  match instr with
  | RType {opc: opcode; rd: register; rs1: register; rs2: register} ->
      ParseUtil.funNotation "RType" ([fst opc] @ List.map regToString [rd; rs1; rs2])
  | IArith {opc: opcode; rd: register; rs1: register; imm: immediate} ->
      ParseUtil.funNotation "IArith" ([fst opc] @ List.map regToString [rd; rs1] @ [fst imm])
  | Csr {opc: opcode; rd: register; rs1: register; imm: immediate} ->
      ParseUtil.funNotation "Csr" ([fst opc] @ List.map regToString [rd; rs1] @ [fst imm])
  | Load {opc: opcode; rd: register; rs1: register; imm: immediate} ->
      ParseUtil.funNotation "Load" ([fst opc] @ List.map regToString [rd; rs1] @ [fst imm])
  | Store {opc: opcode; rs1: register; rs2: register; imm: immediate} ->
      ParseUtil.funNotation "Store" ([fst opc] @ List.map regToString [rs1; rs2] @ [fst imm])
  | Branch {opc: opcode; rs1: register; rs2: register; imm: immediate} ->
      ParseUtil.funNotation "Branch" ([fst opc] @ List.map regToString [rs1; rs2] @ [fst imm])
  | Jal {opc: opcode; rd: register; imm: immediate} ->
      ParseUtil.funNotation "Jal" ([fst opc] @ List.map regToString [rd] @ [fst imm])
  | Jalr {opc: opcode; rd: register; rs1: register; imm: immediate} ->
      ParseUtil.funNotation "Jalr" ([fst opc] @ List.map regToString [rd; rs1] @ [fst imm])
  | UType {opc: opcode; rd: register; imm: immediate} ->
      ParseUtil.funNotation "UType" ([fst opc] @ [regToString rd] @ [fst imm])
  | Label (s, noncify) ->
      "Label(" ^ s ^ " " ^ string_of_bool noncify ^ ")"
let instrCategory instr =
  match instr with
  | RType _ ->
      "rtype"
  | IArith _ ->
      "iarith"
  | Csr _ ->
      "csr"
  | Load _ ->
       "load"
  | Store _ ->
      "store"
  | Branch _ ->
      "control-flow"
  | Jal _ ->
      "control-flow"
  | Jalr _ ->
      "control-flow"
  | UType _ ->
      "utype"
  | Label _ ->
      "control-flow"
let rec finishedBlockToStringInternal fb = finishedBlockContentToStringInternal fb.content

and finishedBlockContentToStringInternal content =
  match content with
  | Instruction instr ->
      instrToString instr
  | MetaBlock mb ->
      List.fold_left ( ^ ) "" (mb |> List.map finishedBlockToStringInternal)

let asmToStringInternal asm =
  match asm with
  | Fragment s ->
      ParseUtil.funNotation "Fragment" [s]
  | Block {top; middle; bottom} ->
      ParseUtil.funNotation "Block"
        [top; finishedBlockContentToStringInternal (MetaBlock middle); bottom]
  | FinishedBlock fb ->
      finishedBlockToStringInternal fb
