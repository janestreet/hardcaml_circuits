open! Base
open Hardcaml

(* Counter will wrap after reaching [max_quotient] AND [max_remainder]. *)
module type Config = sig
  val max_quotient : int
  val max_remainder : int
  val divisor : int
end

module Make (Config : Config) = struct
  include Config

  let () = assert (max_remainder < divisor)
  let quotient_bits = Bits.num_bits_to_represent max_quotient
  let remainder_bits = Bits.num_bits_to_represent (divisor - 1)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; set : 'a
      ; set_quotient : 'a [@bits quotient_bits]
      ; set_remainder : 'a [@bits remainder_bits]
      ; increment : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { quotient : 'a [@bits quotient_bits]
      ; remainder : 'a [@bits remainder_bits]
      }
    [@@deriving hardcaml]
  end

  let create
    _scope
    ({ clock; clear; set; set_quotient; set_remainder; increment } : _ I.t)
    : _ O.t
    =
    let open Signal in
    let spec = Reg_spec.create ~clock ~clear () in
    let wrap_now = wire 1 in
    let counter_remainder =
      reg_fb spec ~width:remainder_bits ~f:(fun x ->
        (* Power of 2 optimisations (excluding divisor = 1). *)
        let wrap_to_0 =
          if Int.is_pow2 divisor && divisor <> 1
          then
            if max_remainder = divisor - 1
            then (* We automatically go to 0 just by incrementing. *)
              gnd
            else increment &: wrap_now
          else increment &: (x ==:. divisor - 1 |: wrap_now)
        in
        Signal.priority_select_with_default
          [ { valid = set; value = set_remainder }
          ; { valid = wrap_to_0; value = zero remainder_bits }
          ; { valid = increment; value = x +:. 1 }
          ]
          ~default:x)
    in
    let counter_quotient =
      let can_skip_wrap =
        Int.is_pow2 divisor
        && max_remainder = divisor - 1
        && Int.is_pow2 (max_quotient + 1)
      in
      reg_fb spec ~width:quotient_bits ~f:(fun x ->
        (* Power of 2 optimisations. *)
        [ Some { With_valid.valid = set; value = set_quotient }
        ; Option.some_if
            (not can_skip_wrap)
            { With_valid.valid = increment &: wrap_now; value = zero quotient_bits }
        ; Some
            { valid = increment &: (counter_remainder ==:. divisor - 1); value = x +:. 1 }
        ]
        |> List.filter_opt
        |> Signal.priority_select_with_default ~default:x)
    in
    wrap_now
    <-- (counter_remainder ==:. max_remainder &: (counter_quotient ==:. max_quotient));
    { quotient = counter_quotient; remainder = counter_remainder }
  ;;

  let hierarchical ?instance (scope : Scope.t) (i : _ I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"counter_div_mod" create i
  ;;
end
