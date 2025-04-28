(** Register stage with up and downstream [valid]/[ready] control. *)

open Base
open Hardcaml

module Make (Data : Hardcaml.Interface.S) : sig
  module IO : sig
    type 'a t =
      { data : 'a Data.t
      ; valid : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; i : 'a IO.t
      }
    [@@deriving hardcaml]
  end

  val create_io : Signal.Reg_spec.t -> Signal.t IO.t -> Signal.t IO.t
  val create : Scope.t -> Signal.t I.t -> Signal.t IO.t
  val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t IO.t
end
