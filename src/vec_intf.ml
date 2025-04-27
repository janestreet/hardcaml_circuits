(** A simple vector structure with insert and delete operations at arbitrary positions. *)

open Base
open Hardcaml

module type Arg = sig
  (** Interface representing the data held in the vec. *)
  module Interface : Interface.S

  (** Construct a register spec for each field in the interface based on index. *)
  val spec : index:int -> Signal.t Interface.t
end

module type S = sig
  type t
  type data

  (** Operation performed on the [vec] circuit. *)
  type op =
    { slot : Signal.t (** Slot to perform operation at *)
    ; op : Signal.t (** Operation type (insert, remove or nothing) *)
    ; insert_data : data (** Data to insert into the vec *)
    ; delete_data : data (** Data written to the emptied slot in the vec *)
    }

  (** Create the vec with the given size.

      [next] defines a function which can set the register when not performing an insert
      or delete operation. *)
  val create
    :  Signal.Reg_spec.t
    -> vec_size:int
    -> next:(index:int -> data -> data)
    -> op
    -> t

  (** Get the value at the given index. *)
  val get : t -> index:int -> data

  (** Create a read multiplexer *)
  val read_mux : t -> index:Signal.t -> data

  (** Size of vec *)
  val vec_size : t -> int
end

module type Vec = sig
  module type Arg = Arg
  module type S = S

  (** {2 operations} *)

  (** no op *)
  val noop : (module Comb.S with type t = 'a) -> 'a

  (** insert at index *)
  val insert : (module Comb.S with type t = 'a) -> 'a

  (** remove at index *)
  val remove : (module Comb.S with type t = 'a) -> 'a

  (** {2 Construction of vec circuits with given size} *)

  module Make (Arg : Arg) : S with type data := Signal.t Arg.Interface.t
end
