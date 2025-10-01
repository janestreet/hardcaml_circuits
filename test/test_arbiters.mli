open! Core
open Hardcaml

type arbiter =
  clock:Signal.t
  -> clear:Signal.t
  -> index:Signal.t Hardcaml_circuits.Arbiters.Index.t
  -> data:(Signal.t, Signal.t) Comb.with_valid2 list
  -> Signal.t With_valid.t

val create_round_robin_seq
  :  num_sources:int
  -> data_width:int
  -> use_mask:bool
  -> arbiter
  -> Cyclesim.t_port_list

val test_round_robin_seq
  :  num_sources:int
  -> data_width:int
  -> Cyclesim.t_port_list
  -> Hardcaml_waveterm.Waveform.t
