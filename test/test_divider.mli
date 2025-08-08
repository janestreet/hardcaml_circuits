open Core
open Hardcaml
open Hardcaml_circuits

module Make_test (Spec : Divider.Spec) : sig
  module Div : module type of Divider.Make (Spec)
  module Sim : module type of Cyclesim.With_interface (Div.I) (Div.O)

  module I_stim : sig
    type t =
      { n : int
      ; d : int
      ; start : bool
      }
    [@@deriving sexp_of]
  end

  val quickcheck_test_setup
    : (Hardcaml_waveterm.Waveform.t * Sim.t * Bits.t ref Div.I.t * Bits.t ref Div.O.t)
        Lazy.t

  val quickcheck_gen : float -> I_stim.t Quickcheck.Generator.t

  val quickcheck_div_iterative
    :  Sim.t
    -> Bits.t ref Div.I.t
    -> Bits.t ref Div.O.t
    -> I_stim.t
    -> unit
end

val spec : int -> Signedness.t -> Divider.Architecture.t -> (module Divider.Spec)
val test_widths : int list
