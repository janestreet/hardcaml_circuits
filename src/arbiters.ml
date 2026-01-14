open! Base
open! Hardcaml

let valids (d : _ With_valid.t list) = List.map d ~f:(fun v -> v.valid)
let values (d : _ With_valid.t list) = List.map d ~f:(fun v -> v.value)

let rotate_by_index
  (type a)
  (module Bits : Comb.S with type t = a)
  ~(index : a)
  ~(data : a With_valid.t list)
  =
  let open Bits in
  (* rotate by the index *)
  let data_width = width (List.hd_exn data).value in
  let valid_list = log_shift ~f:rotr (valids data |> concat_lsb) ~by:index in
  let value_list =
    log_shift
      ~f:(fun d ~by:n -> rotr d ~by:(n * data_width))
      (values data |> concat_lsb)
      ~by:index
  in
  List.map2_exn
    (bits_lsb valid_list)
    (split_lsb ~exact:true ~part_width:data_width value_list)
    ~f:(fun valid value -> { With_valid.valid; value })
;;

module Index = struct
  type 'a t =
    | Offset of 'a
    | Mask of 'a

  let to_valid_bits (type t) (module Bits : Comb.S with type t = t) ~index ~valid =
    let open Bits in
    let num_sources = width valid in
    let create mask =
      let extended_mask = ~:mask @: mask in
      let extended_valid = valid @: valid in
      extended_mask &: extended_valid
    in
    match index with
    | Offset index ->
      let mask : t = log_shift ~f:sll (ones num_sources) ~by:index in
      create mask
    | Mask mask -> create mask
  ;;

  let to_count (type t) (module Bits : Comb.S with type t = t) ~index =
    match index with
    | Offset index -> index
    | Mask mask -> Bits.trailing_zeros mask
  ;;

  let next_mask mask =
    let open Signal in
    if width mask = 1
    then vdd
    else mux2 mask.:(width mask - 2) (sll mask ~by:1) (ones (width mask))
  ;;
end

let select_next_with_clz
  (type a)
  (module Bits : Comb.S with type t = a)
  ~(index : a Index.t)
  (valid : a)
  =
  let open Bits in
  let num_sources = width valid in
  let valid = Index.to_valid_bits (module Bits) ~index ~valid in
  Modulo.unsigned_by_constant (module Bits) (trailing_zeros valid) num_sources
;;

module Round_robin_with_priority = struct
  type 'a combinational =
    (module Comb.S with type t = 'a)
    -> index:'a Index.t (** Prefer [Offset] *)
    -> data:'a With_valid.t list
    -> 'a With_valid.t

  type sequential =
    clock:Signal.t
    -> clear:Signal.t
    -> index:Signal.t Index.t
    -> data:Signal.t With_valid.t list
    -> Signal.t With_valid.t

  module Log_shift = struct
    let combinational
      (type a)
      (module Bits : Comb.S with type t = a)
      ~(index : a Index.t)
      ~(data : a With_valid.t list)
      =
      rotate_by_index (module Bits) ~index:(Index.to_count (module Bits) ~index) ~data
      |> Bits.priority_select
    ;;

    let sequential ~clock ~clear ~index ~data =
      let open Signal in
      let spec = Reg_spec.create ~clock ~clear () in
      rotate_by_index (module Signal) ~index:(Index.to_count (module Signal) ~index) ~data
      |> List.map ~f:(With_valid.map ~f:(reg spec ~enable:vdd))
      |> priority_select
    ;;
  end

  module Count_zeros = struct
    let combinational
      (type a)
      (module Bits : Comb.S with type t = a)
      ~(index : a Index.t)
      ~(data : a With_valid.t list)
      =
      let open Bits in
      let valids = valids data |> concat_lsb in
      let index = select_next_with_clz (module Bits) ~index valids in
      { With_valid.valid = valids <>:. 0; value = mux index (values data) }
    ;;

    let sequential ~clock ~clear ~index ~data =
      let open Signal in
      let spec = Reg_spec.create ~clock ~clear () in
      let reg = reg spec ~enable:vdd in
      let valids = valids data |> concat_lsb in
      let index = select_next_with_clz (module Signal) ~index valids in
      { With_valid.valid = reg (valids <>:. 0); value = mux (reg index) (values data) }
    ;;
  end

  module Onehot_cleaner = struct
    let onehot_selector
      (type a)
      (module Bits : Comb.S with type t = a)
      ~(index : a Index.t)
      ~(data : a With_valid.t list)
      =
      let open Bits in
      let valid = valids data |> concat_lsb in
      let valid = Index.to_valid_bits (module Bits) ~index ~valid in
      let { Onehot_clean.any_bit_set; data } =
        Onehot_clean.scan_from_lsb (module Bits) valid
      in
      let x, y = split_in_half_msb data in
      any_bit_set, x |: y |> bits_lsb
    ;;

    let onehot_data_mux
      (type a)
      (module Bits : Comb.S with type t = a)
      onehot_selector
      data
      =
      let open Bits in
      List.map2_exn onehot_selector data ~f:(fun valid { With_valid.valid = _; value } ->
        mux2 valid value (zero (width value)))
      |> tree ~arity:2 ~f:(reduce ~f:( |: ))
    ;;

    let combinational
      (type a)
      (module Bits : Comb.S with type t = a)
      ~(index : a Index.t)
      ~(data : a With_valid.t list)
      =
      let is_onehot, onehot_selector = onehot_selector (module Bits) ~index ~data in
      let value = onehot_data_mux (module Bits) onehot_selector data in
      { With_valid.valid = is_onehot; value }
    ;;

    let sequential ~clock ~clear ~index ~data =
      let open Signal in
      let spec = Reg_spec.create ~clock ~clear () in
      let reg = reg spec ~enable:vdd in
      let is_onehot, onehot_selector = onehot_selector (module Signal) ~index ~data in
      let is_onehot, onehot_selector = reg is_onehot, List.map onehot_selector ~f:reg in
      let value = onehot_data_mux (module Signal) onehot_selector data in
      { With_valid.valid = is_onehot; value }
    ;;
  end

  module type S = sig
    val combinational : 'a combinational
    val sequential : sequential
  end

  let rec get_arch (arch : Architecture.t) (data : 'a list) : (module S) =
    let is_pow2 = List.length data |> Int.is_pow2 in
    match arch with
    | Small ->
      (* dont use [Count_zeros] for non-power of 2 number of inputs as it requires a
         modulo operation. *)
      if is_pow2 then (module Count_zeros : S) else get_arch Balanced data
    | Balanced -> (module Onehot_cleaner : S)
    | Fast -> (module Log_shift : S)
  ;;

  let combinational
    (type a)
    ?(arch = Architecture.default)
    (module Bits : Comb.S with type t = a)
    ~(index : a Index.t)
    ~(data : a With_valid.t list)
    =
    let module A = (val get_arch arch data) in
    A.combinational (module Bits) ~index ~data
  ;;

  let sequential ?(arch = Architecture.default) () ~clock ~clear ~index ~data =
    let module A = (val get_arch arch data) in
    A.sequential ~clock ~clear ~index ~data
  ;;
end
