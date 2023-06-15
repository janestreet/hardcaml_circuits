(** Synchronous stack implementation *)

open Hardcaml

module Make (Config : sig
    module M : Hardcaml.Interface.S

    val capacity : int
  end) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr_data : 'a Config.M.t
      ; push : 'a
      ; pop : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module M_with_valid : Interface.S with type 'a t = ('a, 'a Config.M.t) With_valid.t2

  module O : sig
    type 'a t =
      { q : 'a M_with_valid.t
      ; full : 'a
      ; empty : 'a
      ; used : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  (** A stack read latency of at least 1. It provides write-before-read semantics, so if
      there is a read and write on the same cycle, the read will return the newly written
      value. *)
  val create : ?read_latency:int -> Scope.t -> Signal.t I.t -> Signal.t O.t

  val hierarchical
    :  ?instance:string
    -> ?read_latency:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
