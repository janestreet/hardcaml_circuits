(** [dividend / (base ** power)], for a given range of [power]s. [base] is an unsigned
    integer constant. [power>0] means we perform an division. [power<0] means
    multipliation.

    When dividing, the output [quotient] (ie the result) is correct if and only if the
    input value is exactly divisible by the divider. It does not perform truncation.

    Only odd values of [base] are currently supported. Even values can be synthesized with
    additional logic to divide by powers of 2 (ie shifting).

    The implementation is based on methods described in:

    "Division by Invariant Integers using Multiplication" by Granlund and Montgomery.
    (https://gmplib.org/~tege/divcnst-pldi94.pdf)

    Optional (but generally required) outputs indicate if the division (or multiplication)
    overflowed the output range, or if the input was not exactly divisible. To support
    this the internal multiply operation uses more bits. *)

open! Base
open Hardcaml

(** A module to represent inclusive ranges of integers. Specifically, it is used in this
    context to represent bounds of exponents. *)
module Inclusive_integer_range : sig
  type t = private
    { min : int
    ; max : int
    }
  [@@deriving sexp_of]

  (** Create an integer range. [min <= max] or it will raise. *)
  val create : min:int -> max:int -> t

  (** Does the range need a sign bit? *)
  val need_sign_bit : t -> bool

  (** Compute the number of bits required to represent this range of values. *)
  val num_bits_to_represent : t -> int
end

type inverses =
  { inverse_bits : int
  ; rom_values : Bigint.t list
  }

module Make (Bits : Comb.S) : sig
  (** Number of bits in [pow] control signals for the given range. *)
  val pow_bits : bounds:Inclusive_integer_range.t -> int

  (** In the first stage we look up the table of modulo inverses corresponding to [pow]. *)
  module Inverse_rom : sig
    type 'a t

    val create
      :  check_for_error:bool
           (** Whether or not this module should internally drive [error] This performs
               two checks -

               (1) Whether the output of the division will fit in [output_bits].
               (2) Whether the [dividend] is an integer multiple of [base ** pow].

               Setting to [true] makes the internal multiply wider and adds some extra
               logic. *)
      -> signedness:Signedness.t (** The dividend signed or unsigned *)
      -> bounds:Inclusive_integer_range.t
           (** The range of powers of [base] that this module supports dividing by. *)
      -> base:int
           (** The base of the power that is going to serve as the divisor in the
               division. *)
      -> output_bits:int
      -> dividend:Bits.t (** The signed number to be divided *)
      -> pow:Bits.t (** The selected power. Requires [pow_bits]. *)
      -> Bits.t t

    val map : Bits.t t -> f:(string -> Bits.t -> Bits.t) -> Bits.t t
  end

  (** In the second stage the multiplication is performed. The quotient result is
      available here. *)
  module Multiplication : sig
    type 'a t

    val create : Bits.t Inverse_rom.t -> Bits.t t
    val map : Bits.t t -> f:(string -> Bits.t -> Bits.t) -> Bits.t t

    (* Accessors *)

    val quotient : Bits.t t -> Bits.t
  end

  (** In the final stage, error checks are performed. *)
  module Error_check : sig
    type 'a t

    val create : Bits.t Multiplication.t -> Bits.t t
    val map : Bits.t t -> f:(string -> Bits.t -> Bits.t) -> Bits.t t

    (* Accessors *)

    val quotient : 'a t -> 'a
    val is_division : 'a t -> 'a
    val error : 'a t -> 'a
    val division_input_too_big : 'a t -> 'a
    val division_output_not_multiple : 'a t -> 'a
    val multiplication_output_too_big : 'a t -> 'a
  end

  val divide
    :  ?map_inverse_rom:(string -> Bits.t -> Bits.t)
    -> ?map_multiplication:(string -> Bits.t -> Bits.t)
    -> ?map_error_check:(string -> Bits.t -> Bits.t)
    -> check_for_error:bool
    -> signedness:Signedness.t
    -> bounds:Inclusive_integer_range.t
    -> base:int
    -> output_bits:int
    -> dividend:Bits.t
    -> pow:Bits.t
    -> unit
    -> Bits.t Types.With_valid.t
end

(** Expose the underlying number theoretic functions to be tested. *)
module For_testing : sig
  (** Compute the maximum positive value that can be represented with [width] bits. *)
  val abs_max : width:int -> Bigint.t

  (** Find the inverse of the given value, mod [modulo]. *)
  val mod_inverse : modulo:Bigint.t -> Bigint.t -> Bigint.t

  (** Build a lookup table of modular inverses (mod [1 lsl output_bits]) for powers of
      [base] in the given range [bounds]. *)
  val inverse_and_bound_lookup
    :  check_for_error:bool
    -> bounds:Inclusive_integer_range.t
    -> base:int
    -> output_bits:int
    -> inverses
end
