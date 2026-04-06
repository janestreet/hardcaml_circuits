open Hardcaml

(** A module for creating balanced fanout trees for clear signals.

    This module provides a pattern for creating fanout trees where you can:
    1. Create a tree with [create]
    2. Add nodes as needed with [node]
    3. Finalize the tree with [finalize] to wire everything up *)

module Make (Clocking : Clocking.S) : sig
  type t

  (** Create a new fanout tree. *)
  val create : Signal.t Clocking.t -> t

  (** Get a new clocking node from the tree. Each call creates a new leaf in the fanout
      tree. *)
  val node : t -> Signal.t Clocking.t

  (** Returns the root clocking (the clear will include any optional logic to drive the
      clear from [finalize] below). Useful when chaining a clear through multiple modules
      and you want a version of the clocking without any pipelining. *)
  val root : t -> Signal.t Clocking.t

  (** Finalize the tree by creating the actual fanout tree and wiring up all nodes.

      [max_fanout]: The maximum fanout of each node of the tree. Defaults to 500.

      [latency]: The latency of the fanout tree. Defaults to ceil(log2(number of nodes)).

      [reg_name_prefix]: The prefix for register names in the fanout tree.

      [clear_signal]: Optional function that takes the clear signal and produces the final
      clear signal to be fanned out. This allows you to combine the clear with other
      signals (e.g., heartbeat timeouts, config resets, etc.) before creating the fanout
      tree. If not provided, the clear signal from [clocking] is used directly. *)
  val finalize
    :  ?max_fanout:int
    -> ?latency:int
    -> ?clear_signal:(Signal.t -> Signal.t)
    -> Scope.t
    -> reg_name_prefix:string
    -> t
    -> unit
end
