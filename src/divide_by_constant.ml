open Base
open Hardcaml

module Make (Bits : Comb.S) = struct
  open Bits

  type parameters =
    { a : Bigint.t
    ; k : int
    }

  let ceil_div x y = Bigint.((x + (y - one)) / y)
  let two = Bigint.of_int 2

  let ceil_log2 x =
    if Bigint.(x < one)
    then raise_s [%message "[clog2] argument must be 1 or more"]
    else (
      let rec f value count =
        if Bigint.(value > zero) then f Bigint.(value / two) (count + 1) else count
      in
      f Bigint.(x - one) 0)
  ;;

  let num_bits_to_represent x = ceil_log2 Bigint.(x + one)

  let parameters ~dividend_bits ~divisor =
    let clog2_d = num_bits_to_represent divisor in
    let k = dividend_bits + clog2_d in
    let a = Bigint.(ceil_div (one lsl k) divisor - (one lsl dividend_bits)) in
    { a; k }
  ;;

  let dividend_larger_than_divisor divisor dividend_bits =
    let divisor_bits = num_bits_to_represent divisor in
    if dividend_bits < divisor_bits
    then
      raise_s
        [%message
          "Divisor is larger than largest possible dividend - result is always zero"]
  ;;

  module Multiply_stage = struct
    type t =
      { b : Bits.t
      ; dividend : Bits.t
      ; k : int
      }

    let create ~divisor dividend =
      let dividend_bits = width dividend in
      dividend_larger_than_divisor divisor dividend_bits;
      let { a; k } = parameters ~dividend_bits ~divisor in
      let a = of_bigint ~width:(num_bits_to_represent a) a in
      let b =
        drop_bottom (dividend *: a) ~width:dividend_bits |> uresize ~width:dividend_bits
      in
      { b; dividend; k }
    ;;

    let map { b; dividend; k } ~f = { b = f b; dividend = f dividend; k }
  end

  module Add_stage = struct
    let create { Multiply_stage.b; dividend; k } =
      let dividend_bits = width dividend in
      drop_bottom (srl (dividend -: b) ~by:1 +: b) ~width:(k - dividend_bits - 1)
    ;;
  end

  let divide ~divisor dividend =
    Multiply_stage.create ~divisor dividend |> Add_stage.create
  ;;
end
