open! Core
open Hardcaml

val tagging_sim : log_vec_size:int -> Cyclesim.t_port_list

val test_tagging
  :  log_vec_size:int
  -> Cyclesim.t_port_list
  -> Hardcaml_waveterm.Waveform.t
