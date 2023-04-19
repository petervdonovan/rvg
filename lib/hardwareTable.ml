open Assembly

let cyclesOf i =
  match i with
  | RType _ ->
      Some 1
  | IArith _ ->
      Some 1
  | Csr _ ->
      Some 1
  | Load _ ->
      Some 2
  | Store _ ->
      Some 1
  | Branch _ ->
      None
  | Jal _ ->
      None
  | Jalr _ ->
      None
  | UType _ ->
      Some 1
  | Label _ ->
      None
