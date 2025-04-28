(** Unsigned division by an constant. Result is exact with respect to standard integer
    division with truncation. Uses a multipler and 2 adders internally.

    https://en.wikipedia.org/wiki/Division_algorithm#Division_by_a_constant *)

open Base
open Hardcaml

module Make (Bits : Comb.S) : sig
  type parameters =
    { a : Bigint.t
    ; k : int
    }

  val parameters : dividend_bits:int -> divisor:Bigint.t -> parameters

  (** Perform initial multiplication stage. [map] can be used to insert a register. *)
  module Multiply_stage : sig
    type t

    val create : divisor:Bigint.t -> Bits.t -> t
    val map : t -> f:(Bits.t -> Bits.t) -> t
  end

  (** Perform final two additions. *)
  module Add_stage : sig
    val create : Multiply_stage.t -> Bits.t
  end

  (** [Multiply_stage |> Add_stage] *)
  val divide : divisor:Bigint.t -> Bits.t -> Bits.t
end
