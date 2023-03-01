include Base
include Hardcaml
include Hardcaml_circuits
include Expect_test_helpers_base

let num_bits = Signal.num_bits_to_represent
let concat = String.concat
let incr = Int.incr

module Float2 = struct
  let string_of_float f =
    Float.round_significant ~significant_digits:2 f |> Float.to_string
  ;;

  let sexp_of_float f = Sexp.Atom (f |> string_of_float)
  let sexp_of_log10 f = Sexp.Atom (concat [ "1E"; Float.log10 f |> string_of_float ])
end
