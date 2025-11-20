open! Base
open Hardcaml
open Signal

module Make (M : Hardcaml.Interface.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr_data : 'a M.t [@rtlprefix "wr$"]
      ; wr_enable : 'a
      ; rd_enable : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { rd_data : 'a M.t [@rtlprefix "rd$"]
      ; rd_valid : 'a
      ; full : 'a
      ; one_from_full : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let create ~cut_through ~capacity (scope : Scope.t) (i : _ I.t) =
    (* Fifo from [Fifo.create] has a one-cycle write latency. This means that writes on
       cycle [T] will be available immediately at cycle [T+1].
    *)
    let fifo_empty = wire 1 in
    let wr_underlying_fifo =
      if cut_through
      then i.wr_enable &: (~:fifo_empty |: ~:(i.rd_enable))
      else i.wr_enable
    in
    let underlying_fifo =
      (* Explicitly tell vivado to use registers rather than any kind of RAM, because this
         fifo should generally be small.
      *)
      Fifo.create
        ~scope
        ~ram_attributes:[ Rtl_attribute.Vivado.Ram_style.registers ]
        ~overflow_check:true
        ~underflow_check:true
        ~showahead:true
        ~capacity
        ~clock:i.clock
        ~clear:i.clear
        ~wr:wr_underlying_fifo
        ~d:(M.Of_signal.pack i.wr_data)
        ~rd:(~:fifo_empty &: i.rd_enable)
        ()
    in
    fifo_empty <-- underlying_fifo.empty;
    let rd_data =
      if cut_through
      then
        M.Of_signal.mux2
          underlying_fifo.empty
          i.wr_data
          (M.Of_signal.unpack underlying_fifo.q)
      else M.Of_signal.unpack underlying_fifo.q
    in
    let rd_valid =
      if cut_through
      then ~:(i.clear) &: (~:(underlying_fifo.empty) |: i.wr_enable)
      else ~:(i.clear) &: ~:(underlying_fifo.empty)
    in
    { O.full = underlying_fifo.full
    ; rd_data
    ; rd_valid
    ; one_from_full = underlying_fifo.nearly_full
    }
  ;;

  let hierarchical
    ?instance
    ?(name = "fast_fifo")
    ~cut_through
    ~capacity
    (scope : Scope.t)
    (i : _ I.t)
    =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name (create ~cut_through ~capacity) i
  ;;
end
