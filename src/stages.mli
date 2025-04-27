(** Represents a computation broken into multiple stages. Each stage is returned in an
    array.

    Used to implement generic register pipelining structures. *)

open Base
open Hardcaml

type 'a t =
  { input : 'a
  ; output : 'a
  }
[@@deriving sexp_of]

(** Function called to construct each stage. It receives the current stage index, the
    output from the previous stage and produces the next stage input. *)
type 'a stage_fn = int -> 'a -> 'a

(** Create an array of stages. The returned array carries both the input and output data
    for each stage. *)
val create : int -> init:'a -> f:'a stage_fn -> 'a t array

(** The initial input of the stages construction. *)
val input : 'a t array -> 'a

(** The final output of the stages construction. *)
val output : 'a t array -> 'a

(** Create a pipeline of registers. The [enable] controls the whole pipeline. *)
val pipeline
  :  Signal.Reg_spec.t
  -> int
  -> enable:Signal.t
  -> init:Signal.t
  -> f:Signal.t stage_fn
  -> Signal.t t array

type enabled_stage =
  { enable : Signal.t
  ; data : Signal.t
  }

(** Create a pipeline of registers. The [enable] is passed down the pipeline with the
    data. *)
val pipeline_with_enable
  :  Signal.Reg_spec.t
  -> int
  -> enable:Signal.t
  -> init:Signal.t
  -> f:Signal.t stage_fn
  -> enabled_stage t array
