(** A vector like structure with insert and delete operations at arbitrary positions.

    This table stores indexes and optional tags and should be connected to a RAM. The
    structure is built from registers. For [N] slots, it requires [N * log2 N * width tag]
    register bits. *)

open Base
open Hardcaml

module type Arg = sig
  (** Tag associated with each index. Set to [Hardcaml.Interface.None] to disable. *)
  module Tag : Interface.S

  (** Value associated with tag values. *)
  val spec : index:int -> Signal.t Tag.t

  (** Size of the vec. *)
  val vec_size : int
end

module type S = sig
  type t
  type tag

  (** {2 Hardware design and queries}

      The vec is a small, growable array of elements. Elements may be inserted and removed
      from any position. Note that insertion and deletion shuffle elements up/down.

      Unlike a normal data structure, it isn't supposed to hold the actual data. Instead
      it holds the index at which the data should reside. It should be connected to an
      external ram with the address connected to:

      - [insertion_index] when inserting
      - [deletion_index] when deleting
      - [access_index] (with the address on [op.slot] and [op.op=noop]) when you want to
        access the data at a slot.

      To insert an element, set [op.op = insert] and *during the same cycle* write the
      data to the (external) memory at [insertion_index]. Similarly to delete an element.

      In some cases it is necessary to associate some extra bits that move with the
      indices - this can be done using tags. Tags may be changed (though the [tag_next]
      function) so long as the circuit is not currently inserting or deleting. To modify
      the tag value during insertion or deletion, see [insertion_tag] and [deletion_tag]
      in the type [Make_tagged.op] below. *)

  (** Return an array of current indexes *)
  val indexes : t -> Signal.t array

  (** Get the index for the given position in the vec *)
  val index : at:Signal.t -> t -> Signal.t

  (** Return an array of current tags *)
  val tags : t -> tag array

  (** Get the tag for the given position in the vec *)
  val tag : at:Signal.t -> t -> tag

  (** Index at which insertion will take place. It is valid _while_ the insert op is
      performed. *)
  val insertion_index : t -> Signal.t

  (** Index at which deletion will take place. It is valid _while_ the delete op is
      performed. *)
  val deletion_index : t -> Signal.t

  (** Index corresponding to [op.slot]. *)
  val access_index : t -> Signal.t

  (** {2 Length status}

      The length is tracked during inserts and removes. To compute the length it tracks
      the insertion/deletion slot. Removing from an 'empty' slot does not decrease the
      size.

      Using length is optional and probably not required unless treating the vec as a
      queue or stack (which can also be implemented more efficiently with a fifo like
      structure).

      [full] and [empty] are derived combinationally from [length]. *)

  (** Length of the vec *)
  val length : t -> Signal.t

  (** Is the vec full? *)
  val full : t -> Signal.t

  (** Is the vec empty? *)
  val empty : t -> Signal.t
end

module type Index_vec = sig
  module type Arg = Arg
  module type S = S

  (** Index vector circuit which tracks indexes and arbitrary user tags *)
  module Make_tagged (Arg : Arg) : sig
    include S with type tag := Signal.t Arg.Tag.t

    (** Operation performed on the [vec] circuit. *)
    type op =
      { slot : Signal.t (** Slot to perform operation at *)
      ; op : Signal.t (** Operation type (insert, remove or nothing) *)
      ; insertion_tag : Signal.t Arg.Tag.t option
      (** Value to set tag to on insertion. If [None] the value associated with the free
          slot moving in is kept *)
      ; deletion_tag : Signal.t Arg.Tag.t option
      (** Value to set the tag to on deletion. If [None] the value associated with the
          slot being kicked out is kept *)
      }

    (** Create the vec with the given size.

        The tag values may be arbitrarily updated with [tag_next] on cycles in which no
        insertion or deletion is taking place ([tag_next] is similar the the callback
        argument provided to [reg_fb] - it takes the current value and optionally modifies
        it to produce the next value). *)
    val create
      :  ?index_next:(index:int -> Signal.t -> Signal.t)
      -> tag_next:(index:int -> Signal.t Arg.Tag.t -> Signal.t Arg.Tag.t)
      -> Signal.Reg_spec.t
      -> op
      -> t
  end

  (** Index vector circuit with tracks indexes. *)
  module Make (X : sig
      val vec_size : int
    end) : sig
    include S with type tag := Signal.t Interface.Empty.t

    (** Operation performed on the [vec] circuit. *)
    type op =
      { slot : Signal.t (** Slot to perform operation at *)
      ; op : Signal.t (** Operation type (insert, remove or nothing) *)
      }

    (** Create the vec with the given size.

        [index_next] is an optional function that can be used to update the index values
        themselves in cycles which no insertion or deletion is taking place. This is
        useful for multiplexing vector operations over data stored in RAM. *)
    val create
      :  ?index_next:(index:int -> Signal.t -> Signal.t)
      -> Signal.Reg_spec.t
      -> op
      -> t
  end
end
