include Base
include Hardcaml

let print_endline = Stdio.print_endline
let print_s sexp = print_endline (Sexp.to_string_hum sexp)
