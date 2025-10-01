(** Register stage with up and downstream [valid]/[ready] control. *)

open Base
open Hardcaml

module M_creates (IO : Interface.S) (I : Interface.S) = struct
  module type S = sig
    val create_io
      :  ?attributes:Rtl_attribute.t list
      -> Signal.Reg_spec.t
      -> Signal.t IO.t
      -> Signal.t IO.t

    val create
      :  ?attributes:Rtl_attribute.t list
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t IO.t

    val hierarchical
      :  ?instance:string
      -> ?attributes:Rtl_attribute.t list
      -> Scope.t
      -> Signal.t I.t
      -> Signal.t IO.t
  end
end

module type Datapath_register = sig
  module M_creates = M_creates

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

    include M_creates(IO)(I).S
  end
end
