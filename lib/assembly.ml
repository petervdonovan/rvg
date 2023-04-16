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
type r_format = { opc: opcode; rd: register; rs1: register; rs2: register }
type i_format = { opc: opcode; rd: register; rs1: register; imm: immediate }
type s_or_b_format = { opc: opcode; rs1: register; rs2: register; imm: immediate }
type u_or_j_format = { opc: opcode; rd: register; imm: immediate }
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
let rec finishedBlockToStringInternal fb = finishedBlockContentToStringInternal fb.content
and finishedBlockContentToStringInternal content =
  match content with
  | Instruction inst -> (match inst with
    | RType inst -> inst.opc |> fst
    | IArith inst -> inst.opc |> fst
    | Csr inst -> inst.opc |> fst
    | Load inst -> inst.opc |> fst
    | Store inst -> inst.opc |> fst
    | Branch inst -> inst.opc |> fst
    | Jal inst -> inst.opc |> fst
    | Jalr inst -> inst.opc |> fst
    | UType inst -> inst.opc |> fst
    | Label (s, _) -> s)
  | MetaBlock mb -> List.fold_left (^) "" (mb |> List.map finishedBlockToStringInternal)
let asmToStringInternal asm =
  match asm with
  | Fragment s -> ParseUtil.funNotation "Fragment" [s]
  | Block {top; middle; bottom} -> ParseUtil.funNotation "Block"
    [top; finishedBlockContentToStringInternal (MetaBlock middle); bottom]
  | FinishedBlock fb -> finishedBlockToStringInternal fb
