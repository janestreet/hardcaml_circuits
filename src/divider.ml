open! Base
open Hardcaml
open Signal
include Divider_intf

module Make (Spec : Spec) = struct
  (* Signed division requires one additional bit in the output, because (INT_MIN)/(-1) is
     not representable in two's complement without one extra bit in the quotient. *)
  let div_width =
    match Spec.signedness with
    | Signed -> Spec.width + 1
    | Unsigned -> Spec.width
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; numerator : 'a [@bits Spec.width]
      ; denominator : 'a [@bits Spec.width]
      ; start : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { quotient : 'a [@bits div_width]
      ; remainder : 'a [@bits div_width]
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  (* {v
        from: https://en.wikipedia.org/wiki/Division_algorithm#Non-restoring_division

        R := N
        D := D << n            -- R and D need twice the word width of N and Q
        for i = n − 1 .. 0 do  -- for example 31..0 for 32 bits
          if R >= 0 then
            q(i) := +1
            R := 2 * R − D
          else
            q(i) := −1
            R := 2 * R + D
          end if
        end

        -- Note: N=numerator, D=denominator, n=#bits, R=partial remainder, q(i)=bit #i of quotient.

        Q := Q − bit.bnot(Q)      -- Appropriate if −1 digits in Q are represented as zeros as is common.

        if R < 0 then
          Q := Q − 1
          R := R + D  -- Needed only if the remainder is of interest.
        end if
     v}
  *)

  module State = struct
    (* R and D need twice the word width of N and Q - add an extra bit to ensure initial
       values stay unsigned *)
    type 'a t =
      { quot : 'a [@bits div_width]
      ; rem : 'a [@bits (2 * div_width) + 1]
      ; denom : 'a [@bits (2 * div_width) + 1]
      ; valid : 'a
      ; count : 'a [@bits address_bits_for div_width]
      ; running : 'a
      ; quot_mask : 'a [@bits div_width]
      }
    [@@deriving hardcaml]

    let create_next_stage scope ?(pipe = Fn.id) t i =
      let ( -- ) = Scope.naming scope in
      let rem_pos = ~:(msb t.rem) -- [%string "remainder_pos-s%{i#Int}"] in
      let denom = pipe t.denom -- [%string "denom-s%{i#Int}"] in
      let rem_x2 = sll t.rem ~by:1 in
      let rem =
        pipe (mux2 rem_pos (rem_x2 -: t.denom) (rem_x2 +: t.denom))
        -- [%string "rem-s%{i#Int}"]
      in
      let quot_mask =
        (sll (of_int_trunc ~width:div_width 1) ~by:i |> reverse)
        -- [%string "quot_mask-s%{i#Int}"]
      in
      let quot =
        pipe (mux2 (rem_pos &: t.valid) (t.quot |: quot_mask) t.quot)
        -- [%string "quot-s%{i#Int}"]
      in
      let valid = pipe t.valid in
      { t with quot; rem; denom; valid }
    ;;
  end

  let create_unsigned_unrolled scope ?(pipe = Fn.id) (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let rec create_pipeline t (stage : int) =
      if stage = div_width
      then t
      else create_pipeline (State.create_next_stage scope ~pipe t stage) (stage + 1)
    in
    let (stage_input : _ State.t) =
      { (State.Of_signal.zero ()) with
        quot = of_int_trunc 0 ~width:div_width
      ; rem = gnd @: zero div_width @: uresize ~width:div_width i.numerator
      ; denom = gnd @: uresize ~width:div_width i.denominator @: zero div_width
      ; valid = i.start
      }
    in
    let pipeline_outputs = create_pipeline stage_input 0 in
    let remainder_neg = msb pipeline_outputs.rem -- "remainder_neg" in
    let binary_quotient =
      pipeline_outputs.quot -: ~:(pipeline_outputs.quot) -- "bin_quot"
    in
    let adjusted_quotient = mux2 remainder_neg (binary_quotient -:. 1) binary_quotient in
    let adjusted_remainder =
      mux2
        remainder_neg
        (pipeline_outputs.rem +: pipeline_outputs.denom)
        pipeline_outputs.rem
    in
    { O.remainder = adjusted_remainder.:+[div_width, Some div_width]
    ; quotient = adjusted_quotient
    ; valid = pipeline_outputs.valid
    }
  ;;

  let create_unsigned_simple
    scope
    ({ clock; clear; numerator; denominator; start } : _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock ~clear () in
    let state = State.Of_always.reg spec in
    State.Of_always.apply_names ~prefix:"st_" ~naming_op:( -- ) state;
    let remainder_pos = ~:(msb state.rem.value) in
    Always.(
      compile
        [ if_
            start
            [ state.denom
              <-- gnd @: uresize ~width:div_width denominator @: zero div_width
            ; state.quot <--. 0
            ; state.rem <-- gnd @: zero div_width @: uresize ~width:div_width numerator
            ; state.count <--. div_width - 1
            ; state.running <--. 1
            ; state.valid <--. 0
            ; state.quot_mask <-- vdd @: zero (width state.quot_mask.value - 1)
            ]
          @@ elif
               state.running.value
               [ state.count <-- state.count.value -:. 1
               ; state.quot_mask <-- srl state.quot_mask.value ~by:1
               ; state.running <-- (state.count.value <>:. 0)
               ; state.valid <-- (state.count.value ==:. 0)
               ; if_
                   remainder_pos
                   [ state.quot <-- (state.quot.value |: state.quot_mask.value)
                   ; state.rem <-- sll state.rem.value ~by:1 -: state.denom.value
                   ]
                   [ state.rem <-- sll state.rem.value ~by:1 +: state.denom.value ]
               ]
               []
        ]);
    let binary_quotient = state.quot.value -: ~:(state.quot.value) in
    let adjusted_quotient = mux2 remainder_pos binary_quotient (binary_quotient -:. 1) in
    let adjusted_remainder =
      mux2 remainder_pos state.rem.value (state.rem.value +: state.denom.value)
    in
    { O.remainder = adjusted_remainder.:+[div_width, Some div_width]
    ; quotient = adjusted_quotient
    ; valid = state.valid.value
    }
  ;;

  let create_unsigned scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    match Spec.architecture with
    | Iterative -> create_unsigned_simple scope i
    | Pipelined -> create_unsigned_unrolled ~pipe:(Signal.reg spec) scope i
    | Combinational -> create_unsigned_unrolled scope i
  ;;

  let create_signed
    (scope : Scope.t)
    ({ clock; clear; numerator; denominator; start } : _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock ~clear () in
    (* Split sign and magnitude -> convert to unsigned *)
    let num_i_sign = msb numerator in
    let denom_i_sign = msb denominator in
    let num_denom_output_signs =
      match Spec.architecture with
      | Iterative -> Signal.reg spec ~enable:start (num_i_sign @: denom_i_sign)
      | Pipelined -> Signal.pipeline spec ~n:div_width (num_i_sign @: denom_i_sign)
      | Combinational -> num_i_sign @: denom_i_sign
    in
    let twos_neg x = ~:x +:. 1 in
    let num_mag = mux2 num_i_sign (twos_neg numerator) numerator -- "num_mag" in
    let denom_mag = mux2 denom_i_sign (twos_neg denominator) denominator -- "denom_mag" in
    let (unsigned_in : _ I.t) =
      { start; clock; clear; numerator = num_mag; denominator = denom_mag }
    in
    (* Do unsigned divide *)
    let num_o_sign = msb num_denom_output_signs -- "num_o_sign" in
    let denom_o_sign = lsb num_denom_output_signs -- "denom_o_sign" in
    let unsigned_out = create_unsigned scope unsigned_in in
    (* Apply sign rules to result *)
    let signed_quot =
      mux2
        (num_o_sign ^: denom_o_sign)
        (twos_neg unsigned_out.quotient)
        unsigned_out.quotient
    in
    let signed_rem =
      mux2 num_o_sign (twos_neg unsigned_out.remainder) unsigned_out.remainder
    in
    { O.remainder = signed_rem; quotient = signed_quot; valid = unsigned_out.valid }
  ;;

  let create (scope : Scope.t) (i : _ I.t) =
    match Spec.signedness with
    | Signed -> create_signed scope i
    | Unsigned -> create_unsigned scope i
  ;;

  let hierarchical ?instance ?(name = "divider") (scope : Scope.t) (i : _ I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name create i
  ;;
end
