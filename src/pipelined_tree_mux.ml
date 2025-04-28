open Base
open Hardcaml

(* [min_sequence_sums_to ~value ~in_num_steps] computes a sequence of integers of length
   [in_num_steps] which when summed equals [value]. *)
let min_sequence_sums_to ~value ~in_num_steps =
  let rec f num_steps bits =
    if num_steps = 0
    then []
    else if bits = 0
    then 0 :: f (num_steps - 1) 0
    else if num_steps > bits
    then 1 :: f (num_steps - 1) (bits - 1)
    else (
      let d = bits / num_steps in
      let r = bits % num_steps in
      let d = d + if r <> 0 then 1 else 0 in
      d :: f (num_steps - 1) (bits - d))
  in
  if in_num_steps < 1
  then raise_s [%message "[min_sequence_sums_to] num steps must be >= 1"];
  if value < 1 then raise_s [%message "[min_sequence_sums_to] value must be >= 1"];
  f in_num_steps value
;;

(* This is a complicated way of saying [pipeline ~n:cycles (mux select_byte state)]. The
   difference is that the registers are balanced throughout the multiplexer tree. *)
let pipelined_tree_mux ~cycles ~reg ~selector state =
  let rec f sel data = function
    | [] -> raise_s [%message "[pipelined_tree_mux] no steps"]
    | [ _ ] ->
      if List.length data = 1
      then reg (List.hd_exn data)
      else (
        match sel with
        | None -> raise_s [%message "[pipelined_tree_mux] No select bits"]
        | Some sel -> Signal.mux sel data |> reg)
    | bits :: bs ->
      let sel_hi, sel_lo =
        match sel with
        | None -> None, None
        | Some sel ->
          if bits = 0
          then Some sel, None
          else if Signal.width sel = bits
          then None, Some sel
          else
            ( Some (Signal.drop_bottom sel ~width:bits)
            , Some (Signal.sel_bottom sel ~width:bits) )
      in
      let l = List.chunks_of data ~length:(1 lsl bits) in
      f
        (Option.map sel_hi ~f:reg)
        (List.map l ~f:(fun l ->
           match l with
           | [ hd ] -> reg hd
           | l ->
             (match sel_lo with
              | None -> raise_s [%message "No select low bits"]
              | Some sel_lo -> Signal.mux sel_lo l |> reg)))
        bs
  in
  let num_data = List.length state in
  if num_data = 0 then raise_s [%message "[pipelined_tree_mux] no data elements"];
  let range_selector = 1 lsl Signal.width selector in
  if num_data > range_selector
  then
    raise_s
      [%message
        "[pipeline_tree_mux] not all data elements can be indexed by the selector"];
  let bits = Int.ceil_log2 num_data in
  let steps = min_sequence_sums_to ~value:bits ~in_num_steps:cycles in
  f (Some selector) state steps
;;

let pipelined_tree_priority_select
  ?(trace_reductions = false)
  ?(pipelined_enable = Signal.vdd)
  ~cycles
  ~(reg : ?enable:Signal.t -> Signal.t -> Signal.t)
  data
  =
  if cycles < 0
  then
    raise_s
      [%message
        "pipelined_tree_priority_select cannot accept negative [cycles] argument"
          (cycles : int)]
  else if cycles = 0
  then Signal.priority_select data
  else (
    let length = List.length data in
    let rec search_for_reduction_factor i =
      if Int.pow i cycles > length then i else search_for_reduction_factor (i + 1)
    in
    let reduction_factor = search_for_reduction_factor 2 in
    let rec reduce ~cycle ~enable data =
      if cycle = cycles
      then (
        match data with
        | [ hd ] -> hd
        | _ ->
          raise_s
            [%message
              "Expecting singleton list after reductions"
                (cycles : int)
                (reduction_factor : int)
                ~reduced_length:(List.length data : int)
                ~input_length:(length : int)])
      else
        (let l = List.chunks_of data ~length:reduction_factor in
         if trace_reductions
         then (
           let reductions = List.map l ~f:(List.length :> _ -> _) in
           Stdio.print_s [%message (cycle : int) (reductions : int list)]);
         List.map l ~f:(fun l ->
           With_valid.map (Signal.priority_select l) ~f:(reg ~enable)))
        |> reduce
             ~cycle:(cycle + 1)
             ~enable:
               (if Signal.Type.is_vdd enable
                then
                  (* Skip inserting a register when enable is just tied to vdd. *)
                  enable
                else reg ~enable:Signal.vdd enable)
    in
    reduce ~cycle:0 ~enable:pipelined_enable data)
;;
