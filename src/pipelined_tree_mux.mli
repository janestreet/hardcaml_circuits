open Hardcaml

val pipelined_tree_mux
  :  cycles:int
  -> reg:(Signal.t -> Signal.t)
  -> selector:Signal.t
  -> Signal.t list
  -> Signal.t

val pipelined_tree_priority_select
  :  ?trace_reductions:bool (** For debugging in the expect tests *)
  -> cycles:int
  -> reg:(Signal.t -> Signal.t)
  -> Signal.t With_valid.t list
  -> Signal.t With_valid.t
