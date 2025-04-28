open Base
open Hardcaml
open Signal

module type Arg = Vec_intf.Arg
module type S = Vec_intf.S

let noop (type a) (module S : Comb.S with type t = a) : a = S.of_string "00"
let insert (type a) (module S : Comb.S with type t = a) : a = S.of_string "10"
let remove (type a) (module S : Comb.S with type t = a) : a = S.of_string "11"

module Make (Arg : Arg) = struct
  type t =
    { vec_size : int
    ; regs : Signal.t Arg.Interface.t array
    }
  [@@deriving fields ~getters]

  type op =
    { slot : Signal.t
    ; op : Signal.t
    ; insert_data : Signal.t Arg.Interface.t
    ; delete_data : Signal.t Arg.Interface.t
    }

  let create spec ~vec_size ~next op =
    let do_insert = op.op ==: insert (module Signal) in
    let do_remove = op.op ==: remove (module Signal) in
    let reg_wires = Array.init vec_size ~f:(fun _ -> Arg.Interface.Of_signal.wires ()) in
    let regs =
      Array.init vec_size ~f:(fun index ->
        Arg.Interface.map2 (Arg.spec ~index) reg_wires.(index) ~f:(fun value d ->
          (* Including [reset_to] here would be slightly more general. However, we only
             really use sync clears, and it changes rtl_checksums (due to uid labelling,
             not actual logic differences) so we avoid it for now. *)
          reg ~clear_to:value ~enable:vdd spec d))
    in
    let op_index_1h = binary_to_onehot op.slot in
    let shifted_up =
      Array.init vec_size ~f:(fun index ->
        if index = 0 then regs.(0) else regs.(index - 1))
    in
    let shifted_down =
      Array.init vec_size ~f:(fun index ->
        if index = vec_size - 1 then op.delete_data else regs.(index + 1))
    in
    let shift = log_shift ~f:sll (ones vec_size) ~by:op.slot in
    for index = 0 to vec_size - 1 do
      let shift = shift.:(index) in
      Arg.Interface.iter2
        reg_wires.(index)
        (let mux2 = Arg.Interface.Of_signal.mux2 in
         mux2
           (do_insert &: shift)
           (mux2 op_index_1h.:(index) op.insert_data shifted_up.(index))
           (mux2 (do_remove &: shift) shifted_down.(index) (next ~index regs.(index))))
        ~f:( <-- )
    done;
    { vec_size; regs }
  ;;

  let get t ~index = t.regs.(index)
  let read_mux t ~index = Arg.Interface.Of_signal.mux index (Array.to_list t.regs)
end
