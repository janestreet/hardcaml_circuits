(** Pipelined adder architectures for very wide adders. *)

open Base
open Hardcaml

(** Pipelined adder. The result is available after
    [(adder_width + partwidth) / part_width] cycles. *)
val create
  :  part_width:int
  -> clock:Signal.t
  -> ?clear:Signal.t
  -> ?c_in:Signal.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

(** Pipelined adder with a constant 2 cycle latency.

    Florent de Dinechin, Hong Diep Nguyen, Bogdan Pasca. Pipelined FPGA Adders.
    International Conference on Field Programmable Logic and Applications, Aug 2010,
    Milano, Italy. pp.422-427, ff10.1109/FPL.2010.87ff. ffensl-00475780v2f

    There is an output combinational delay of a single adder:

    [width / part_width].

    The maximum delay is an adder of width:

    [max (width / part_width) (part_width*2)] *)
module Short_latency : sig
  (** Combinational version of the circuit. For testing. *)
  val comb : (module Comb.S with type t = 'a) -> part_width:int -> 'a -> 'a -> 'a

  (** Sequential circuit with 2 cycle delay. *)
  val create
    :  part_width:int
    -> clock:Signal.t
    -> ?clear:Signal.t
    -> Signal.t
    -> Signal.t
    -> Signal.t
end
