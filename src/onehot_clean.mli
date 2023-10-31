(** Convert an arbitrary input vector to onehot. The first bit set scanning from either
    the lsb or msb will be set in the output and all others will be 0. The architecture
    has logarithmic delay.

    examples (with scan_from_msb):

    - [1111] -> [1000]
    - [0000] -> [0000]
    - [0110] -> [0100] *)

open! Hardcaml

type 'a t =
  { any_bit_set : 'a
  ; data : 'a
  }
[@@deriving hardcaml]

val scan_from_msb : (module Comb.S with type t = 'a) -> 'a -> 'a t
val scan_from_lsb : (module Comb.S with type t = 'a) -> 'a -> 'a t
