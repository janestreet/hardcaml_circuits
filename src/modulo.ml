open! Base
open! Hardcaml

(* We calculate the max bits the result could be (based on the subtraction of y) and
   resize the result to this. *)
let unsigned_by_constant' (type a) (module S : Comb.S with type t = a) (x : a) (y : int) =
  assert (y > 0);
  let rec build y =
    if y > 1 lsl S.width x
    then x
    else (
      let x = build (y lsl 1) in
      let res = S.mux2 S.(x >=:. y) S.(x -:. y) x in
      S.uresize res ~width:(Bits.num_bits_to_represent y))
  in
  build y
;;

let unsigned_by_constant (type a) (module S : Comb.S with type t = a) (x : a) (y : int) =
  if y = 0
  then raise_s [%message "Cannot perform mod 0"]
  else if y = 1
  then S.zero (S.width x)
  else if Int.is_pow2 y
  then x.S.:[Int.ceil_log2 y - 1, 0]
  else unsigned_by_constant' (module S) x y
;;
