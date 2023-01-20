let isWhitespace char = List.mem char [' '; '\n'; '\t'; '\r']
let funNotation name args = name ^ "(" ^ (String.concat ", " args) ^ ")"
let debug_print description value = print_endline (description ^ value)
let nonce () =
  let nonce1 () = 74 |> Random.int |> (+) 48 in
  Seq.forever nonce1 |> Seq.filter (fun x -> x < 58 || (x > 64 && x < 91) || x > 96) |> Seq.map Char.chr |> Seq.take 10 |> (
  fun s -> let b = Buffer.create 16 in
  Seq.iter (Buffer.add_char b) s;
  Buffer.contents b)

let%expect_test _ = (Random.init 0; print_string (nonce ())); [%expect{| RNYh2Z6mVs |}]
