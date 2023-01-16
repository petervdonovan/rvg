let isWhitespace char = List.mem char [' '; '\n'; '\t'; '\r']
let funNotation name args = name ^ "(" ^ (String.concat ", " args) ^ ")"
let debug_print description value = print_endline (description ^ value)
