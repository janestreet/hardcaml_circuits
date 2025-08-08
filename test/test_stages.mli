open! Core
open! Hardcaml

val sim : unit -> Hardcaml_waveterm.Waveform.t * Cyclesim.t_port_list
val run : Cyclesim.t_port_list -> unit
val sim_with_enable : unit -> Hardcaml_waveterm.Waveform.t * Cyclesim.t_port_list
val run_with_enable : Cyclesim.t_port_list -> unit
