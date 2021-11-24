open Hardcaml

val pipelined_tree_mux
  :  cycles:int
  -> reg:(Signal.t -> Signal.t)
  -> selector:Signal.t
  -> Signal.t list
  -> Signal.t
