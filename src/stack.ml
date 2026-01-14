open Base
open Hardcaml
open Signal

module type Config = sig
  module M : Hardcaml.Interface.S

  val capacity : int
end

module Make (Config : Config) = struct
  open Config

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr_data : 'a M.t
      ; push : 'a
      ; pop : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module M_with_valid = With_valid.Wrap.Make (M)

  let bits_for_addr = address_bits_for capacity

  module O = struct
    type 'a t =
      { q : 'a M_with_valid.t [@rtlprefix "q$"]
      ; full : 'a
      ; empty : 'a
      ; used : 'a [@bits bits_for_addr + 1]
      (* We need 1 more bit for [used] to encode values up to and including [capacity] *)
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let create ?(read_latency = 1) (scope : Scope.t) (i : _ I.t) =
    if read_latency < 1
    then raise_s [%message "stack read latency must be >= 1" (read_latency : int)];
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let empty = wire 1 -- "empty" in
    let full = wire 1 -- "full" in
    (* decode operation *)
    let push_actual = (i.push &: ~:(i.pop) &: ~:full) -- "push_actual" in
    let pop_actual = (~:(i.push) &: i.pop &: ~:empty) -- "pop_actual" in
    let cut_through = (i.push &: i.pop) -- "cut_through" in
    (* size tracking *)
    let used = wire (bits_for_addr + 1) -- "used" in
    let used_minus_1 = wire bits_for_addr -- "used_minus_1" in
    let used_next = mux2 push_actual (used +:. 1) (mux2 pop_actual (used -:. 1) used) in
    used <-- reg spec used_next;
    used_minus_1 <-- reg spec (lsbs (used_next -:. 1));
    let empty_next = used_next ==:. 0 in
    empty <-- reg spec ~clear_to:vdd ~reset_to:Bits.vdd empty_next;
    let full_next = used_next ==:. capacity in
    full <-- reg spec full_next;
    (* ram instantiation *)
    let write_port =
      { Write_port.write_clock = i.clock
      ; write_address = lsbs used
      ; write_data = M.Of_signal.pack i.wr_data
      ; write_enable = push_actual
      }
    in
    let read_port =
      { Read_port.read_clock = i.clock; read_address = used_minus_1; read_enable = vdd }
    in
    let ram =
      let ram_arr =
        Ram.create
          ~attributes:[ Rtl_attribute.Vivado.Ram_style.block ]
          ~collision_mode:Read_before_write
          ~size:capacity
          ~write_ports:[| write_port |]
          ~read_ports:[| read_port |]
          ()
      in
      M.Of_signal.unpack ram_arr.(0) |> M.Of_signal.pipeline spec ~n:(read_latency - 1)
    in
    (* create output *)
    let q =
      let n = read_latency in
      let wr_data_pipe = M.Of_signal.pipeline spec ~n i.wr_data in
      let cut_through_pipe = pipeline spec ~n cut_through in
      M_with_valid.Of_signal.mux2
        cut_through_pipe
        (* cut through - output is always valid, just the previous input pipelined *)
        { With_valid.valid = vdd; value = wr_data_pipe }
        (* read from ram - only valid when the ram isn't empty *)
        { With_valid.valid = pipeline spec ~n pop_actual; value = ram }
    in
    { O.q; full; empty; used }
  ;;

  let hierarchical ?instance ?read_latency (scope : Scope.t) (i : _ I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"stack" (create ?read_latency) i
  ;;
end
