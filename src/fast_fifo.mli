(** Low latency combinational fifo. See [create] for documentation. *)

open Hardcaml

module Make (M : Hardcaml.Interface.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr_data : 'a M.t
      ; wr_enable : 'a
      ; rd_enable : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { rd_data : 'a M.t
      ; rd_valid : 'a
      ; full : 'a
      ; one_from_full : 'a
      }
    [@@deriving hardcaml]
  end

  (** Creates a combinational, showahead fifo. Namely, [rd_valid] will be asserted on the
      same cycle that data is written into the fifo. The data will be held at the output
      of the fifo until [read_enable] is asserted.

      The actual capacity of the fifo is [capacity + 1], due to additional registering.
      Note that when [o.full] is asserted, [i.wr_enable] is ignored, even if [rd_enable]
      is asserted at the same cycle.

      Raises an exception if [capacity < 1]. *)
  val create : cut_through:bool -> capacity:int -> Scope.t -> Signal.t I.t -> Signal.t O.t

  val hierarchical
    :  ?instance:string
    -> ?name:string
    -> cut_through:bool
    -> capacity:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
