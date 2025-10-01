open! Core
open Hardcaml

val sim
  :  cycles:int
  -> num_data:int
  -> Hardcaml_waveterm.Waveform.t * Cyclesim.t_port_list

val run : Cyclesim.t_port_list -> cycles:int -> num_data:int -> unit
