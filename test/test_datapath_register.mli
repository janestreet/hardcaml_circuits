open Core
open Hardcaml
module Data : Interface.S
include module type of Hardcaml_circuits.Datapath_register.Make (Data)

val create_sim
  :  ?all_waves:bool
  -> unit
  -> Hardcaml_waveterm.Waveform.t * (Bits.t ref I.t, Bits.t ref IO.t) Cyclesim.t

val run_test : sim:(Bits.t ref I.t, Bits.t ref IO.t) Cyclesim.t -> int -> unit
