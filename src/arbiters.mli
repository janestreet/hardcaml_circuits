(** Arbiter circuits. Given a set of requesters the arbiter chooses one to succeed. *)

open! Base
open! Hardcaml

(** {v
 Priority is specified as either a offset to start searching from, or as a special mask.

    The mask has the form:

    - [0]             [11..1111]
    - [1]             [11..1110]
    - [2]             [11..1100]
    - [3]             [11..1000]
    - [num_sources-1] [10..0000]
    v} *)
module Index : sig
  type 'a t =
    | Offset of 'a
    | Mask of 'a

  (** Compute the next value of a priority mask register. The initial (reset) value should
      be all ones, though it probably doesn't matter too much if it comes up all zeros as
      it will correctly set itself in the next cycle. *)
  val next_mask : Signal.t -> Signal.t
end

(** Rotate element [index] in [data] to [0] using a [log_shift] circuit. *)
val rotate_by_index
  :  (module Comb.S with type t = 'a)
  -> index:'a
  -> data:'a With_valid.t list
  -> 'a With_valid.t list

(** Find the 1st bit set in the input vector greater than [index], but wrapping at the top
    back to bit 0 and on to [index-1]. *)
val select_next_with_clz
  :  (module Comb.S with type t = 'a)
  -> index:'a Index.t
  -> 'a
  -> 'a

(** Round-robin arbiters with priority.

    These functions take an [index] at which to start searching for the next valid input.
    They will wrap at the top back down to 0 and then on to [index-1]. *)
module Round_robin_with_priority : sig
  type 'a combinational =
    (module Comb.S with type t = 'a)
    -> index:'a Index.t
    -> data:'a With_valid.t list
    -> 'a With_valid.t

  type sequential =
    clock:Signal.t
    -> clear:Signal.t
    -> index:Signal.t Index.t
    -> data:Signal.t With_valid.t list
    -> Signal.t With_valid.t

  module type S = sig
    val combinational : 'a combinational
    val sequential : sequential
  end

  (** Round-robin arbiter implementation which rotates the inputs using [rotate_by_index]
      and then uses [priority_select] to choose the active input. *)
  module Log_shift : S

  (** Round-robin arbiter implementation which uses masks and a [trailing_zeros] counter
      to find the next active input relative to [index] and then a mux to select the data.
      The architecture will work, but be horribly inefficient if the number of input
      sources is not a power of 2. *)
  module Count_zeros : S

  (** Round-robin arbiter implementation which converts the masked priority vector to
      onehot to select the first active source. *)
  module Onehot_cleaner : S

  (* {2 Pre-selected defaults.}

     Defaults to balanced. *)
  val combinational : ?arch:Architecture.t -> 'a combinational
  val sequential : ?arch:Architecture.t -> unit -> sequential
end
