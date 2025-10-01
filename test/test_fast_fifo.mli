open! Core
open Hardcaml

module Foo : sig
  type 'a t =
    { hello : 'a
    ; world : 'a
    }
  [@@deriving hardcaml]
end

module Fast_fifo : module type of Hardcaml_circuits.Fast_fifo.Make (Foo)
module Sim : module type of Cyclesim.With_interface (Fast_fifo.I) (Fast_fifo.O)

val create_sim
  :  ?capacity:int
  -> ?cut_through:bool
  -> unit
  -> Hardcaml_waveterm.Waveform.t * Sim.t

val combinational_read_write : Sim.t -> unit
val read_exact_one_cycle_after_write : Sim.t -> unit
val read_and_write_same_cycle_when_not_empty : Sim.t -> unit
val read_and_write_same_cycle_when_empty : Sim.t -> unit
val write_when_full_not_registered : Sim.t -> unit
val demo_rd_en_1_rd_valid_0_until_available : Sim.t -> unit
