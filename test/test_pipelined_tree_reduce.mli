open! Core
open! Hardcaml

val sim : arity:int -> int -> Hardcaml_waveterm.Waveform.t * Cyclesim.t_port_list
val run : Cyclesim.t_port_list -> int -> unit
