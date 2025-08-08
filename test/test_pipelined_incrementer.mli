open Core
open Hardcaml

type ports =
  { increment : Bits.t ref
  ; set : Bits.t ref
  ; value : Bits.t ref
  ; q : Bits.t ref
  }

val sim
  :  part_width:int
  -> increment_width:int
  -> total_width:int
  -> Cyclesim.t_port_list * Hardcaml_waveterm.Waveform.t * ports

val incr_by_one : sim:Cyclesim.t_port_list -> ports -> unit
val incr_by_three : sim:Cyclesim.t_port_list -> ports -> unit
val incr_by_differing : sim:Cyclesim.t_port_list -> ports -> unit
val set_value : sim:Cyclesim.t_port_list -> ports -> unit
val weird_sizes : sim:Cyclesim.t_port_list -> ports -> unit
val sixty_four_bit : sim:Cyclesim.t_port_list -> ports -> unit
