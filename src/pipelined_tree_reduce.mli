(** Pipelined tree reduce operation, with propogation delay equivalent to
    [ceil(log(|args|))] *)

open Base
open Hardcaml

val ceil_log : base:int -> int -> int

val create
  :  f:(Signal.t -> Signal.t -> Signal.t)
  -> enable:Signal.t
  -> arity:int
  -> Signal.Reg_spec.t
  -> Signal.t list
  -> Signal.t With_valid.t
