open ParseUtil
open Templates
open Assembly

let isEmpty fragment = fragment = ""

let providedLabelsOf fbl =
  let pickFirstIfBoth _ a _ = Some a in
  let unfiltered fbl' =
    List.fold_left
      (Noncification.union pickFirstIfBoth)
      Noncification.empty
      (fbl' |> List.map (fun c -> c.provides))
  in
  let isInstr fb = match fb.content with Instruction _ -> true | _ -> false in
  let instrsOnly = List.filter isInstr fbl in
  let metablocks = List.filter (Fun.negate isInstr) fbl in
  Noncification.union pickFirstIfBoth (instrsOnly |> unfiltered) (metablocks |> unfiltered)

let finishedBlockOf content =
  { content
  ; provides=
      ( match content with
      | MetaBlock mb ->
          providedLabelsOf mb
      | Instruction instr -> (
        match instr with
        | Label (s, noncify) ->
            Noncification.singleton s (if noncify then nonce () else "")
        | _ ->
            Noncification.empty ) )
  ; totalCycles= None
  ; cyclesMod= CyclesModMap.empty }

module NameSet = Set.Make (String)

let strsToNameset strs = List.fold_right NameSet.add strs NameSet.empty

let rTypeInstrs : NameSet.t =
  strsToNameset ["add"; "sub"; "and"; "or"; "xor"; "sll"; "srl"; "sra"; "slt"; "sltu"; "mul"]

let iTypeInstrs =
  strsToNameset ["addi"; "andi"; "ori"; "xori"; "slli"; "srli"; "srai"; "slti"; "sltiu"]

let csrInstrs = strsToNameset ["csrrw"; "csrrs"; "csrrc"; "csrrwi"; "csrrsi"; "csrrci"]

let loadInstrs = strsToNameset ["lb"; "lbu"; "lh"; "lhu"; "lw"]

let storeInstrs = strsToNameset ["sb"; "sh"; "sw"]

let branchInstrs = strsToNameset ["beq"; "bge"; "bgeu"; "blt"; "bltu"; "bne"]

let jalInstrs = strsToNameset ["jal"]

let jalrInstrs = strsToNameset ["jalr"]

let temporaries =
  strsToNameset
    ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"]

let saved = strsToNameset ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"]

let failWithNoBinding name r =
  raise (Eval.AssertionFail ("Could not find binding for name \"" ^ name ^ "\"", r))

let expectAsm r env name description =
  let badExprType bad =
    Eval.AssertionFail
      (description ^ " expected, but found expression " ^ Ast.exprToString bad, (snd bad).r)
  in
  if Eval.Environment.mem name env then
    let e, _ = Eval.evalExpr env (Eval.Environment.find name env) in
    match e with
    | Ast.Integer i, meta ->
        (string_of_int i, meta.r)
    | Ast.Template tem, meta -> (
      match Ast.unwrap (Ast.Template (tem |> Eval.fullyEvalTem)) with
      | Some good ->
          (good, meta.r)
      | None ->
          raise (badExprType e) )
    | Ast.Asm num, meta ->
        (num, meta.r)
    | Ast.ParsedAsm pasm, _ -> (
      match pasm with
      | Fragment f ->
          (f, r)
      | _ ->
          raise (Eval.AssertionFail ("Expected assembly fragment, but got assembly block", r)) )
    | bad ->
        raise (badExprType bad)
  else failWithNoBinding name r

let rec finishedBlockToString fb =
  match fb.content with
  | Instruction i ->
      instrToString i
  | MetaBlock mb ->
      funNotation "MetaBlock" [mb |> List.map finishedBlockToString |> String.concat "\n"]

let rec asmToString asm =
  match asm with
  | Fragment s ->
      funNotation "Fragment" [s]
  | Block {top; middle; bottom} ->
      funNotation "Block"
        [top; asmToString (FinishedBlock (finishedBlockOf (MetaBlock middle))); bottom]
  | FinishedBlock fb ->
      finishedBlockToString fb

let evalToString env str r =
  let e, _ = Eval.evalExpr env (Ast.Name str, Ast.metaInitial r) in
  parseTokenExpectingString env e

let rec nameToReg name =
  let (str, r), env = name in
  if NameSet.mem str temporaries then TempReg (str, r)
  else if NameSet.mem str saved then SaveReg (str, r)
  else if str = "zero" then Zero r
  else if str = "ra" then Ra r
  else if str = "sp" then Sp r
  else if str = "gp" then Gp r
  else if str = "tp" then Tp r
  else
    match evalToString env str r with
    | Some name', _ ->
        nameToReg name'
    | None, _ ->
        raise (Eval.AssertionFail ("expected register but got nothing", r))

let resolveNumericalImm imm =
  let (s, r), env = imm in
  match s |> int_of_string_opt with
  | Some _ ->
      (s, r)
  | None ->
      if Eval.Environment.mem s env then
        match Eval.evalExpr env (Ast.Name s, Ast.metaInitial r) with
        | (Ast.Integer i, _), _ ->
            (i |> string_of_int, r)
        | e, _ ->
            raise (Eval.AssertionFail ("expected number, not " ^ (e |> Ast.exprToString), r))
      else raise (Eval.AssertionFail (s ^ " is not a number and is not bound in the environment", r))

let resolveLabel label =
  let (l, r), env = label in
  if not (Eval.Environment.mem l env) then (l, r)
  else
    match evalToString env l r with
    | Some (l', _), _ ->
        l'
    | None, _ ->
        raise (Eval.AssertionFail ("expected label but got nothing", r))

let asmParseFail formatDescription e r =
  raise
    (Eval.AssertionFail
       ( "Instruction \"" ^ Ast.exprToString e ^ "\" does not follow the instruction syntax for "
         ^ formatDescription
       , r ) )

let parseR env opc e =
  match get3Tokens env e with
  | Some (rd, rs1, rs2), e' ->
      ( Some (Instruction (RType {opc; rd= nameToReg rd; rs1= nameToReg rs1; rs2= nameToReg rs2}))
      , e' )
  | None, _ ->
      (None, Some e)

let parseI env opc e =
  match get3Tokens env e with
  | Some (rd, rs1, imm), e' ->
      ( Some
          (Instruction
             (IArith {opc; rd= nameToReg rd; rs1= nameToReg rs1; imm= resolveNumericalImm imm}) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseLoad env opc e =
  match get3TokensWithThirdTokenInParens env e with
  | Some (rd, imm, rs1), e' ->
      ( Some
          (Instruction
             (Load {opc; rd= nameToReg rd; rs1= nameToReg rs1; imm= resolveNumericalImm imm}) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseStore env opc e =
  match get3TokensWithThirdTokenInParens env e with
  | Some (rs2, imm, rs1), e' ->
      ( Some
          (Instruction
             (Store {opc; rs2= nameToReg rs2; rs1= nameToReg rs1; imm= resolveNumericalImm imm}) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseBranch env opc e =
  match get3Tokens env e with
  | Some (rs1, rs2, label), e' ->
      ( Some
          (Instruction
             (Branch {opc; rs1= nameToReg rs1; rs2= nameToReg rs2; imm= resolveLabel label}) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseJal env opc e =
  match get2Tokens env e with
  | Some (rd, label), e' ->
      (Some (Instruction (Jal {opc; rd= nameToReg rd; imm= resolveLabel label})), e')
  | None, _ ->
      (None, Some e)

let parseJalr env opc e =
  match get3Tokens env e with
  | Some (rd, rs1, imm), e' ->
      ( Some
          (Instruction
             (Jalr {opc; rd= nameToReg rd; rs1= nameToReg rs1; imm= resolveNumericalImm imm}) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseU env opc e =
  match get2Tokens env e with
  | Some (rd, imm), e' ->
      (Some (Instruction (UType {opc; rd= nameToReg rd; imm= fst imm})), e')
  | None, _ ->
      (None, Some e)

let parseBxxz env opc e =
  let opcs, r = opc in
  match get2Tokens env e with
  | Some (rs1, label), e' ->
      ( Some
          (Instruction
             (Branch
                { opc= (if opcs = "beqz" then ("beq", r) else ("bne", r))
                ; rs1= nameToReg rs1
                ; rs2= Zero r
                ; imm= resolveLabel label } ) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseJ env opc e =
  let _, r = opc in
  match parseTokenExpectingString env e with
  | Some (label, r'), e' ->
      (Some (Instruction (Jal {opc= ("jal", r); rd= Zero r; imm= resolveLabel (label, r')})), e')
  | None, _ ->
      (None, Some e)

let parseJr env opc e =
  let _, r = opc in
  match parseTokenExpectingString env e with
  | Some (rs1s, r'), e' ->
      ( Some
          (Instruction
             (Jalr {opc= ("jalr", r); rd= Zero r; rs1= nameToReg (rs1s, r'); imm= ("0", r)}) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseLa _ opc _ = raise (Eval.AssertionFail ("la is not currently supported", snd opc))

let parseLi env opc e =
  let _, r = opc in
  match get2Tokens env e with
  | Some (rd, ((imm, immr), env')), e' ->
      let ks, _ = resolveNumericalImm ((imm, immr), env') in
      let k = int_of_string ks in
      let upper = k / 4096 in
      let lower = k - (4096 * upper) in
      let lower = if lower > 2047 then lower - 4096 else lower in
      let upper = (k - lower) / 4096 in
      let rd = nameToReg rd in
      ( Some
          (MetaBlock
             ( ( if upper = 0 then []
                 else
                   [ finishedBlockOf
                       (Instruction (UType {opc= ("lui", r); rd; imm= (string_of_int upper, immr)}))
                   ] )
             @
             if lower = 0 && upper <> 0 then []
             else
               [ finishedBlockOf
                   (Instruction
                      (IArith
                         { opc= ("addi", r)
                         ; rd
                         ; rs1= (if upper = 0 then Zero r else rd)
                         ; imm= (string_of_int lower, immr) } ) ) ] ) )
      , e' )
  | None, _ ->
      (None, Some e)

let parseMv env opc e =
  let _, r = opc in
  match get2Tokens env e with
  | None, _ ->
      (None, Some e)
  | Some (rd, rs1), e' ->
      ( Some
          (Instruction
             (IArith {opc= ("addi", r); rd= nameToReg rd; rs1= nameToReg rs1; imm= ("0", r)}) )
      , e' )

let parseNeg env opc e =
  let _, r = opc in
  match get2Tokens env e with
  | None, _ ->
      (None, Some e)
  | Some (rd, rs1), e' ->
      ( Some
          (Instruction (RType {opc= ("sub", r); rd= nameToReg rd; rs1= nameToReg rs1; rs2= Zero r}))
      , e' )

let parseNop _ opc e =
  let _, r = opc in
  (Some (Instruction (IArith {opc= ("addi", r); rd= Zero r; rs1= Zero r; imm= ("0", r)})), Some e)

let parseNot env opc s =
  let _, r = opc in
  match get2Tokens env s with
  | None, _ ->
      (None, Some s)
  | Some (rd, rs1), e' ->
      ( Some
          (Instruction
             (IArith {opc= ("xori", r); rd= nameToReg rd; rs1= nameToReg rs1; imm= ("-1", r)}) )
      , e' )

let parseRet _ opc e =
  let _, r = opc in
  (Some (Instruction (Jalr {opc= ("jalr", r); rd= Zero r; rs1= Ra r; imm= ("0", r)})), Some e)

let parseCsrInstr env opc e =
  let opc, r = opc in
  match get3Tokens env e with
  | None, _ ->
      (None, Some e)
  | Some (rd, csr, rs1), e' ->
      ( Some
          (Instruction
             (Csr {opc= (opc, r); rd= nameToReg rd; rs1= nameToReg rs1; imm= resolveNumericalImm csr}
             ) )
      , e' )

let parseCsrw env opc e =
  let _, r = opc in
  match get2Tokens env e with
  | None, _ ->
      (None, Some e)
  | Some (csr, source), e' ->
      ( Some
          (Instruction
             (Csr
                {opc= ("csrrw", r); rd= Zero r; rs1= nameToReg source; imm= resolveNumericalImm csr}
             ) )
      , e' )

let parseCsrr env opc e =
  let _, r = opc in
  match get2Tokens env e with
  | None, _ ->
      (None, Some e)
  | Some (dest, csr), e' ->
      ( Some
          (Instruction
             (Csr {opc= ("csrrs", r); rd= nameToReg dest; rs1= Zero r; imm= resolveNumericalImm csr})
          )
      , e' )

let parseRdcycle env opc e =
  let _, r = opc in
  match parseTokenExpectingString env e with
  | None, _ ->
      (None, Some e)
  | Some ((rd, r'), env'), e' ->
      ( Some
          (Instruction
             (Csr
                {opc= ("csrrs", r'); rd= nameToReg ((rd, r'), env'); rs1= Zero r; imm= ("cycle", r)}
             ) )
      , e' )

let rec renoncify fb =
  match fb.content with
  | Instruction (Label (l, noncifiable)) ->
      { content= fb.content
      ; provides= Noncification.singleton l (if noncifiable then nonce () else "")
      ; totalCycles= fb.totalCycles
      ; cyclesMod= fb.cyclesMod }
  | Instruction _ ->
      { content= fb.content
      ; provides= fb.provides
      ; totalCycles= fb.totalCycles
      ; cyclesMod= fb.cyclesMod }
  | MetaBlock fbl ->
      let fbl' = fbl |> List.map renoncify in
      let provides = providedLabelsOf fbl' in
      {content= MetaBlock fbl'; provides; totalCycles= fb.totalCycles; cyclesMod= fb.cyclesMod}

let tryParse env e =
  let makeFinishedBlock o =
    let o, e' = o in
    (Option.bind o (fun v -> Some (finishedBlockOf v)), e')
  in
  let opcode, env', e' = parseToken e in
  let env = Option.value env' ~default:env in
  let e'' = Option.value e' ~default:(Ast.Asm "", Ast.metaEmpty) in
  match opcode with
  | Some (Asm opcode, meta) ->
      (let pred = NameSet.mem opcode in
       let f =
         if pred rTypeInstrs then parseR
         else if pred iTypeInstrs then parseI
         else if pred loadInstrs then parseLoad
         else if pred storeInstrs then parseStore
         else if pred branchInstrs then parseBranch
         else if pred jalInstrs then parseJal
         else if pred jalrInstrs then parseJalr
         else if pred csrInstrs then parseCsrInstr
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
         else if opcode = "csrr" then parseCsrr
         else if opcode = "rdcycle" then parseRdcycle
         else if String.ends_with ~suffix:":" opcode && String.length opcode > 1 then fun _ _ _ ->
           ( Some
               (Instruction
                  (Label
                     ( String.sub opcode 0 (String.length opcode - 1)
                     , String.uppercase_ascii opcode = opcode ) ) )
           , e' )
         else fun _ _ _ -> (None, Some e)
       in
       try f env (opcode, meta.r) e''
       with Eval.AssertionFail (s, r) ->
         raise (Eval.EvalFail ("error parsing " ^ opcode ^ ": " ^ s, [meta.r; r])) )
      |> makeFinishedBlock
  | Some (ParsedAsm (FinishedBlock fb), _) ->
      (Some (fb |> renoncify), e')
  | Some (_, _) ->
      (None, None)
  | None ->
      (None, None)

let rec parse env e =
  parseRec env (Some e) [] |> List.rev |> fun it -> (finishedBlockOf (MetaBlock it), snd e)

and parseRec env e acc =
  match e with
  | None ->
      acc
  | Some e -> (
    match tryParse env e with
    | Some content, e' ->
        parseRec env e' (content :: acc)
    | None, None ->
        acc
    | None, _ ->
        raise
          (Eval.AssertionFail
             ("Expected assembly in parse but got " ^ (e |> Ast.exprToString), (snd e).r) ) )

let rec print pasm =
  match pasm with
  | Fragment f ->
      print_string f
  | Block b ->
      b.top |> String.trim |> print_string ;
      List.iter (printFinishedBlock []) b.middle ;
      b.bottom |> String.trim |> print_string
  | FinishedBlock fb ->
      printFinishedBlock [] fb

and printFinishedBlock hn fb =
  let hn' = List.cons fb.provides hn in
  match fb.content with
  | Instruction i ->
      print_string (stringifyInstruction hn' i)
  | MetaBlock mb ->
      List.iter (printFinishedBlock hn') mb

and stringifyInstruction hierarchicalNoncifications i =
  let noncify r label =
    try
      let n =
        Noncification.find label (List.find (Noncification.mem label) hierarchicalNoncifications)
      in
      if n = "" then label else label ^ "_" ^ n
    with Not_found ->
      raise
        (Eval.AssertionFail
           ( "Could not find label " ^ label
             ^ " in the current context, and so could not print the label with the correct nonce"
           , r ) )
  in
  let ( $ ) s reg =
    s
    ^
    match reg with
    | TempReg (s, _) ->
        s
    | SaveReg (s, _) ->
        s
    | Zero _ ->
        "zero"
    | Ra _ ->
        "ra"
    | Sp _ ->
        "sp"
    | Gp _ ->
        "gp"
    | Tp _ ->
        "tp"
  in
  let ( $$ ) s (reg : register) = (s ^ "(" $ reg) ^ ")" in
  let ( < ) opc t = "    " ^ fst opc ^ " " $ t in
  let ( $ ) s reg = s ^ ", " $ reg in
  let ( = ) s imm = s ^ ", " ^ fst imm in
  let ( == ) s imm = s ^ ", " ^ (imm |> fst |> noncify (snd imm)) in
  ( match i with
  | RType {opc; rd; rs1; rs2} ->
      opc < rd $ rs1 $ rs2
  | IArith {opc; rd; rs1; imm} ->
      opc < rd $ rs1 = imm
  | Csr {opc; rd; rs1; imm} ->
      opc < rd = imm $ rs1
  | Load {opc; rd; rs1; imm} ->
      opc < rd = imm $$ rs1
  | Store {opc; rs1; rs2; imm} ->
      opc < rs2 = imm $$ rs1
  | Branch {opc; rs1; rs2; imm} ->
      opc < rs1 $ rs2 == imm
  | Jal {opc; rd; imm} ->
      opc < rd == imm
  | Jalr {opc; rd; rs1; imm} ->
      opc < rd $ rs1 = imm
  | UType {opc; rd; imm} ->
      opc < rd = imm
  | Label (s, _) ->
      noncify
        ( {startInclusive= CharStream.origin "TODO"; endExclusive= CharStream.origin "TODO"}
          : CharStream.range )
        s
      ^ ":" )
  ^ "\n"
