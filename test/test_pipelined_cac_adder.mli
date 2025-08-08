open! Core
open Hardcaml

val sim : part_width:int -> Cyclesim.t_port_list
val test : Cyclesim.t_port_list -> unit
