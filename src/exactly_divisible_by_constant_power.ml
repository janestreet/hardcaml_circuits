open! Base
open! Hardcaml

module Inclusive_integer_range = struct
  open Bits

  type t =
    { min : int
    ; max : int
    }
  [@@deriving sexp_of]

  let create ~min ~max =
    if min > max
    then raise_s [%message "Invalid integer range - min > max" (min : int) (max : int)];
    { min; max }
  ;;

  let need_sign_bit { min; max } = min < 0 || max < 0

  let num_bits_to_represent ({ min; max } as t) =
    let num_bits_maybe_signed v =
      if v >= 0
      then
        num_bits_to_represent v (* If the number needs to be signed, add a sign bit. *)
        + if need_sign_bit t then 1 else 0
      else 1 + num_bits_to_represent (-(v + 1))
    in
    Int.max (num_bits_maybe_signed min) (num_bits_maybe_signed max)
  ;;
end

(* Modular Inversion based on Euclid's Algorithm for gcd. Assumes that gcd(v, modulo) = 1
   and raises if this isn't true. *)
let mod_inverse ~modulo v =
  let open Bigint in
  let rec loop a b x y =
    if a < zero
    then failwith "only for positives"
    else if a = zero
    then failwith [%string "gcd(%{v#Bigint}, %{modulo#Bigint}) <> 1"]
    else if a = one
    then y
    else loop b (a % b) (y - (a / b * x)) x
  in
  if v = one
  then one
  else (
    let res = loop v modulo zero one in
    if res < zero then res + modulo else res)
;;

type inverses =
  { inverse_bits : int
  ; rom_values : Bigint.t list
  }

(* The key task is to build a lookup table for the values: [base]^(-N) (mod
   2^[output_i_bits]) for N ∈ [pow_base_bounds].

   We allow the powers to be any number - positive, negative, or zero.

   We also need to compute, for each power, the maximum value V such that [V / base^N]
   fits within [output_i_bits].

   When [check_for_error] is set high, we have to include enough bits in the modular
   inverse to represent the full range of values [2^[output_i_bits] * [base] ^ N] for N ∈
   [pow_base_bounds] (in order to detect when the input is not a multiple of the divisor).
*)
let inverse_and_bound_lookup
  ~check_for_error
  ~(bounds : Inclusive_integer_range.t)
  ~base
  ~output_bits
  =
  let inverse_bits =
    if check_for_error
    then (
      let num_bits_to_represent bound =
        Bits.num_bits_to_represent (Int.pow base (Int.abs bound))
      in
      max (num_bits_to_represent bounds.min) (num_bits_to_represent bounds.max)
      + output_bits)
    else
      (* When we're not checking whether the input is a multiple, we need fewer bits
         because we can allow non-multiple values to alias to valid ones. *)
      output_bits
  in
  let rom_values =
    List.range ~start:`inclusive ~stop:`inclusive bounds.min bounds.max
    |> List.map ~f:(fun pow_base ->
      if pow_base <= 0
      then (
        (* Dividing by [base ^ -pow] is just an integer multiplication. *)
        let inverse = Int.pow base (-pow_base) |> Bigint.of_int in
        inverse)
      else (
        let inverse =
          mod_inverse
            ~modulo:Bigint.(one lsl inverse_bits)
            (Bigint.of_int (Int.pow base pow_base))
        in
        inverse))
  in
  { inverse_bits; rom_values }
;;

module Make (Bits : Comb.S) = struct
  open Bits

  let pow_bits ~bounds = Inclusive_integer_range.num_bits_to_represent bounds
  let pow_signed ~bounds = Inclusive_integer_range.need_sign_bit bounds

  module Inverse_rom = struct
    module T = struct
      type 'a t =
        { inverse : 'a
        ; dividend : 'a
        ; is_division : 'a
        }
      [@@deriving hardcaml]
    end

    type 'a t =
      { bits : 'a T.t
      ; output_bits : int
      ; check_for_error : bool
      ; signedness : Signedness.t
      }

    let create ~check_for_error ~signedness ~bounds ~base ~output_bits ~dividend ~pow =
      if width pow <> pow_bits ~bounds
      then raise_s [%message "Width of [pow] is incorrect"];
      if base % 2 = 0 then raise_s [%message "[base] must be odd"];
      if base < 3 then raise_s [%message "[base] must be 3 or more"];
      let { inverse_bits; rom_values } =
        inverse_and_bound_lookup ~check_for_error ~bounds ~base ~output_bits
      in
      let rom_idx = pow -: of_int_trunc ~width:(Bits.width pow) bounds.min in
      (* Because [Config.pow_base_bounds] don't necessarily start at 0, we also have to
         map from the power we are dividing by to the lookup address in the ROM. *)
      { bits =
          { inverse =
              (let conv = of_bigint ~width:inverse_bits in
               match rom_values with
               | [ inverse ] -> conv inverse
               | _ -> mux rom_idx (List.map rom_values ~f:conv))
          ; dividend
          ; is_division = (if pow_signed ~bounds then pow >+. 0 else pow >:. 0)
          }
      ; output_bits
      ; check_for_error
      ; signedness
      }
    ;;

    let map t ~f = { t with bits = T.map2 T.port_names t.bits ~f }
  end

  module Multiplication = struct
    module T = struct
      type 'a t =
        { dividend : 'a
        ; result : 'a
        ; is_division : 'a
        }
      [@@deriving hardcaml]
    end

    type 'a t =
      { bits : 'a T.t
      ; inverse_bits : int
      ; output_bits : int
      ; check_for_error : bool
      ; signedness : Signedness.t
      }
    [@@deriving fields ~getters]

    let create
      { Inverse_rom.bits = { inverse; dividend; is_division }
      ; output_bits
      ; check_for_error
      ; signedness
      }
      =
      let result =
        match signedness with
        | Signed -> dividend *+ inverse
        | Unsigned -> dividend *: inverse
      in
      { bits = { dividend; result; is_division }
      ; inverse_bits = width inverse
      ; output_bits
      ; check_for_error
      ; signedness
      }
    ;;

    let map t ~f = { t with bits = T.map2 T.port_names t.bits ~f }
    let quotient t = sel_bottom t.bits.result ~width:t.output_bits
  end

  module Error_check = struct
    type 'a t =
      { quotient : 'a
      ; division_input_too_big : 'a
      ; division_output_not_multiple : 'a
      ; multiplication_output_too_big : 'a
      ; is_division : 'a
      ; error : 'a
      }
    [@@deriving hardcaml, fields ~getters]

    (* Return [vdd] if [signed] cannot be represented by a signed [num_bits]-wide integer. *)
    let signed_int_doesn't_fit ~num_bits signed =
      if num_bits < Bits.width signed
      then
        drop_bottom signed ~width:num_bits
        <>: repeat signed.:(num_bits - 1) ~count:(width signed - num_bits)
      else gnd
    ;;

    let unsigned_int_doesn't_fit ~num_bits unsigned =
      if num_bits < Bits.width unsigned
      then drop_bottom unsigned ~width:num_bits <>:. 0
      else gnd
    ;;

    let create
      ({ Multiplication.bits = { dividend; result; is_division }
       ; inverse_bits
       ; output_bits
       ; check_for_error
       ; signedness
       } as t)
      =
      let int_doesn't_fit =
        match signedness with
        | Signed -> signed_int_doesn't_fit
        | Unsigned -> unsigned_int_doesn't_fit
      in
      let division_input_too_big =
        if check_for_error then int_doesn't_fit ~num_bits:inverse_bits dividend else gnd
      in
      let division_output_not_multiple =
        (* Technically, this also catches some corner cases of the input being too big -
           specifically when it's between the max output range and the range representable
           by [inverse_bits]. *)
        if check_for_error
        then int_doesn't_fit ~num_bits:output_bits (sel_bottom result ~width:inverse_bits)
        else gnd
      in
      let multiplication_output_too_big = int_doesn't_fit ~num_bits:output_bits result in
      { quotient = Multiplication.quotient t
      ; division_input_too_big = division_input_too_big &: is_division
      ; division_output_not_multiple = division_output_not_multiple &: is_division
      ; multiplication_output_too_big = multiplication_output_too_big &: ~:is_division
      ; is_division
      ; error =
          mux2
            is_division
            (division_input_too_big |: division_output_not_multiple)
            multiplication_output_too_big
      }
    ;;

    let map t ~f = map2 port_names t ~f
  end

  let divide
    ?(map_inverse_rom = fun _ -> Fn.id)
    ?(map_multiplication = fun _ -> Fn.id)
    ?(map_error_check = fun _ -> Fn.id)
    ~check_for_error
    ~signedness
    ~bounds
    ~base
    ~output_bits
    ~dividend
    ~pow
    ()
    =
    let q =
      Inverse_rom.create
        ~check_for_error
        ~signedness
        ~bounds
        ~base
        ~output_bits
        ~dividend
        ~pow
      |> Inverse_rom.map ~f:map_inverse_rom
      |> Multiplication.create
      |> Multiplication.map ~f:map_multiplication
      |> Error_check.create
      |> Error_check.map ~f:map_error_check
    in
    { With_valid.valid = ~:(Error_check.error q); value = Error_check.quotient q }
  ;;
end

module For_testing = struct
  let abs_max ~width =
    let shift = width - 1 in
    Bigint.((one lsl shift) - one)
  ;;

  let mod_inverse = mod_inverse
  let inverse_and_bound_lookup = inverse_and_bound_lookup
end
