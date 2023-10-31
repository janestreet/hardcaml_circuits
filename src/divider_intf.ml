(** Non-restoring divider. See [create] for documentation. *)

open! Base
open! Hardcaml
module Architecture = Cordic.Architecture

module type Spec = sig
  val width : int
  val signedness : Signedness.t
  val architecture : Architecture.t
end

module type Divider = sig
  module type Spec = Spec

  module Architecture = Architecture

  module Make (Spec : Spec) : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; numerator : 'a
        ; denominator : 'a
        ; start : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { quotient : 'a
        ; remainder : 'a
        ; valid : 'a
        }
      [@@deriving hardcaml]
    end

    module State : sig
      type 'a t =
        { quot : 'a
        ; rem : 'a
        ; denom : 'a
        ; valid : 'a
        ; count : 'a
        ; running : 'a
        ; quot_mask : 'a
        }
      [@@deriving hardcaml]
    end

    (** Creates a non-restoring divider with config specified in [Spec]. The width of the
        input arguments is defined with [Spec.width]. The divider can operate on signed or
        unsigned arguments as defined in [Spec.signed] and performs truncated division
        (which aligns with default OCaml behaviour of [Int.( / )] and [Int.( mod )]).

        The delay of the divider is fixed at [Spec.width] cycles regardless of
        architecture.

        Three divider architectures are supported as set in [Spec.architecture]. In
        [Iterative] mode, only one operation per [Spec.width] cycles is supported. If
        [start] is asserted prior to the completion of the previous result, that operation
        is aborted and the new operation commences. [start] can be asserted on the same
        cycle that [valid] is high.

        In [Pipelined] or [Combinational] mode, one operation can be performed every
        cycle. In this mode [start] informs the divider that the inputs are valid. In
        [Combinational] mode, [valid] is assigned to start and outputs are updated in the
        same cycle. In [Pipelined] mode, [valid] will be asserted [Spec.width] cycles
        after [start] with the result of the divide. *)

    val create : Scope.t -> Interface.Create_fn(I)(O).t

    val hierarchical
      :  ?instance:string
      -> ?name:string
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end
