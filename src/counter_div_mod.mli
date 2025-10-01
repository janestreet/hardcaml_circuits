(** A counter. If the current value is [n], it outputs [n]/[divisor] (as [quotient]) and
    [n] mod [divisor] (as [remainder]). Since the counter doesn't track [n] directly, the
    maximum values must be specified as [max_quotient] and [max_remainder], and the
    counter will wrap when its outputs reach [max_quotient] AND [max_remainder].

    It also has [set] functionality, but again [set_quotient] and [set_remainder] must be
    directly specified.

    It has special cases to simplify HW when [divisor] is a power of 2 and if also
    [max_remainder] = [divisor] - 1, and if also [max_quotient] + 1 is a power of 2. *)

open! Base
open Hardcaml

module type Config = sig
  val max_quotient : int
  val max_remainder : int
  val divisor : int
end

module Make (Config : Config) : sig
  include Config

  val quotient_bits : int
  val remainder_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; set : 'a
      ; set_quotient : 'a
      ; set_remainder : 'a
      ; increment : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { quotient : 'a
      ; remainder : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
