(** Calculate [x % y] where [x >= 0] and [y > 0].

    If [y] is a power of 2, then a fast path calculation is performed that just does a
    [select]. Otherwise, a much (much) more complex circuit is generated. *)

open! Hardcaml

val unsigned_by_constant : (module Comb.S with type t = 'a) -> 'a -> int -> 'a
