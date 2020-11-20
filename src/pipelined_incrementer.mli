(** Simple pipelined incrementer. *)

open Base
open Hardcaml

(** Split the addition into [width / part_width] sub-adders. The increment value must be
    no wider than [part_width].

    The value can be [set] at any time. *)
val create
  :  part_width:int
  -> clock:Signal.t
  -> clear:Signal.t
  -> set:Signal.t With_valid.t
  -> increment:Signal.t
  -> Signal.t
