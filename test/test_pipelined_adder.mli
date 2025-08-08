open! Core
open! Hardcaml

val sim : part_width:int -> adder_width:int -> Cyclesim.t_port_list

val test
  :  sim:Cyclesim.t_port_list
  -> part_width:int
  -> adder_width:int
  -> num_tests:int
  -> unit
