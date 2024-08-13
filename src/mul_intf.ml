open Base
open Hardcaml

module type Gen = sig
  include Add.Gen

  val width : t -> int
  val bit : t -> pos:int -> bit
  val gnd : bit
  val ( +: ) : t -> t -> t
  val uresize : t -> width:int -> t
end

module type Mul = sig
  module type Gen = Gen

  module Config : sig
    type t =
      | Dadda
      | Wallace
    [@@deriving enumerate, sexp_of]
  end

  val create_gen : config:Config.t -> (module Gen with type t = 'a) -> 'a -> 'a -> 'a
  val create : config:Config.t -> (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
end
