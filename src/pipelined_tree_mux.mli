open Hardcaml

val pipelined_tree_mux
  :  cycles:int
  -> reg:(Signal.t -> Signal.t)
  -> selector:Signal.t
  -> Signal.t list
  -> Signal.t

val pipelined_tree_priority_select
  :  ?trace_reductions:bool (** For debugging in the expect tests *)
  -> ?pipelined_enable:Signal.t
       (** Optional register enable that is pipelined along with the data. Defaults to
           [vdd] which means data passes every cycle. *)
  -> cycles:int
  -> reg:(?enable:Signal.t -> Signal.t -> Signal.t)
  -> Signal.t With_valid.t list
  -> Signal.t With_valid.t
