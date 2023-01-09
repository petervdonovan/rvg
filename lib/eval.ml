module Environment = Map.Make(String)

exception EvalFail of string

open ParseUtil
open Assembly

let bindNames env params args =
  let nparams = List.length params in
    let nargs = List.length args in
      if nparams = nargs
        then List.fold_left2
          (fun env param arg ->
            let p : Ast.var = param in
              Environment.add p.name arg env)
          env params args
        else raise (EvalFail ("Expected " ^ (string_of_int nparams) ^ " arguments but got " ^ (string_of_int nargs) ^ " arguments." ))

let rec evalExpr env e = match e with
  | Ast.Name str -> if Environment.mem str env
    then evalExpr env (Environment.find str env)
    else raise (EvalFail ("Unbound name: " ^ str))
  | Ast.Var v -> raise (EvalFail (
    "A parameter is not an expression, but tried to evaluate "
    ^ (Ast.exprToString (Ast.Var v))))
  | Ast.ParsedAsm p -> Ast.ParsedAsm p, env
  | Ast.Asm asm -> Ast.Asm asm, env
  | Ast.Template tem -> let exprs', env' = evalExprListInOrder env tem
        in Ast.Template exprs', env'
  | Ast.Lam lam -> Ast.Lam lam, env
  | Ast.LamApplication { lam; args } ->
    let lam', env' = evalExpr env lam in
      let args', _ = evalExprListInOrder env' args in
        match lam' with
        | Lam { params; value; env } -> evalExpr (bindNames env params args') value
        | e -> raise (EvalFail ("Expected Lam but got " ^ Ast.exprToString e))
and evalExprListInOrder env exprList =
  let exprs, env = List.fold_left
    ( fun (exprs, env) expr ->
      let expr', env' = evalExpr env expr in
        expr' :: exprs, env'
    ) ([], env) exprList
  in List.rev exprs, env

let expectAsm env name description =
  (if Environment.mem name env
    then match evalExpr env (Environment.find name env) with
    | Ast.Asm num, _ -> num
    | bad, _ -> raise (AsmParseFail (
      description ^ " expected, but found expression " ^ Ast.exprToString bad))
    else failWithNoBinding name)
let rec nameToReg env name =
  if NameSet.mem name temporaries then TempReg name
  else if NameSet.mem name saved then SaveReg name
  else if name = "zero" then Zero
  else if name = "ra" then Ra
  else if name = "sp" then Sp
  else if name = "gp" then Gp
  else if name = "tp" then Tp
  else nameToReg env (expectAsm env name "Register name")
let resolveNumericalImm env imm =
  match int_of_string_opt imm with
  | Some _ -> imm
  | None -> expectAsm env imm "Numerical immediate"
let asmParseFail formatDescription s =
  raise (AsmParseFail ("Instruction \"" ^ (charSeq2String s) ^ "\" does not follow the instruction syntax for " ^ formatDescription))
let parseR env opc s =
  match get3Tokens s with
  | Some (rd, rs1, rs2, _) -> RType { opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1; rs2 = nameToReg env rs2 }
  | None -> asmParseFail "R type syntax" s
let parseI env opc s =
    match get3Tokens s with
    | Some (rd, rs1, imm, _) -> (IArith {
      opc = opc; rd = nameToReg env rd; rs1 = nameToReg env rs1;
      imm = resolveNumericalImm env imm
    })
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
      else parseJalr
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
let funNotation name args = name ^ "(" ^ (String.concat ", " args) ^ ")"
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
let printAsm str = (String.to_seq str)
  |> Seq.fold_left (append Environment.empty) empty
  |> asmToString |> print_endline

let%expect_test _ =
  printAsm {|
  add t0 t1 t2
  addi a0 a1 0x66
  lbu s4 12(t6)
  beq a5 t3 END
  jal ra END3

  jalr zero t4 END2
  |};
  [%expect{|
    RType(add, temp-t0, temp-t1, temp-t2)
    IArith(addi, temp-a0, temp-a1, 0x66)
    Load(lbu, save-s4, temp-t6, 12)
    Branch(beq, temp-a5, temp-t3, END)
    Jal(jal, Ra, END3)
    Jalr(jalr, Zero, temp-t4, END2) |}]

let evalProgram exprs = exprs |> evalExprListInOrder Environment.empty |> fst

let printReducedAst text =
  text |> Ast.getAst |> evalProgram |> List.map Ast.exprToString
  |> List.iter print_endline

let%expect_test _ = printReducedAst "[lam [] \"\" ]";
  [%expect{| Lam(params=[], value=Name("")) |}]

let%expect_test _ = printReducedAst "[[lam [] {}]]";
  [%expect{| Template() |}]

let%expect_test _ = printReducedAst "[[lam [(x)] {}] {} ]";
  [%expect{| Template() |}]

let%expect_test _ = printReducedAst "[[lam [(x)] { [[lam [(x)] {y}] {}] x }] { alphabet }]";
  [%expect {| Template(Asm( )Template(Asm(y))Asm( x )) |}]
