open Base
open Hardcaml

module Tree = struct
  type t =
    | Idx of int
    | Subtree of t list
  [@@deriving sexp_of, variants]

  (* This is just [List.chunks_of] except with the last chunk having possibly more items *)
  let chunks_of ~length list =
    let n = List.length list in
    List.groupi list ~break:(fun i _ _ -> i % length = 0 && i <= n - length)
  ;;

  let create_balanced ~leaves ~latency =
    if latency < 0 || leaves <= 0
    then
      raise_s
        [%message
          "[get_balanced] expects ~latency to be >= 0 and ~leaves > 0"
            (leaves : int)
            (latency : int)];
    let layers = latency + 1 in
    let rec inner ~leaf_list ~layers =
      let leaves = List.length leaf_list in
      let branching_factor = Signal.compute_arity ~steps:layers leaves in
      if layers = 1
      then Subtree (List.map leaf_list ~f:idx)
      else (
        let chunk_length = leaves / branching_factor in
        let chunks = chunks_of ~length:chunk_length leaf_list in
        List.map chunks ~f:(fun leaf_list -> inner ~leaf_list ~layers:(layers - 1))
        |> subtree)
    in
    inner ~leaf_list:(List.init leaves ~f:Fn.id) ~layers
  ;;

  let create_from_branching_factors ~branching_factors =
    if List.is_empty branching_factors
    then
      raise_s
        [%message "[create_from_branching_factors] expects non-empty branching_factors"];
    let rec inner ~leaf_list ~branching_factors =
      let leaves = List.length leaf_list in
      match branching_factors with
      | [] ->
        assert (List.length leaf_list = 1);
        Idx (List.hd_exn leaf_list)
      | bf :: bf_tl ->
        let chunk_length = leaves / bf in
        let chunks = chunks_of ~length:chunk_length leaf_list in
        List.map chunks ~f:(fun leaf_list -> inner ~leaf_list ~branching_factors:bf_tl)
        |> subtree
    in
    let leaves = List.fold branching_factors ~init:1 ~f:( * ) in
    inner ~leaf_list:(List.init leaves ~f:Fn.id) ~branching_factors
  ;;

  let rec is_equally_deep ~depth tree =
    if depth < 0
    then raise_s [%message "is_equally_deep expected ~depth to be >= 0" (depth : int)];
    match tree with
    | Idx _ -> depth = 0
    | Subtree subtrees ->
      assert (depth >= 0);
      (match depth with
       | 0 -> false
       | _ ->
         List.for_all subtrees ~f:(fun subtree ->
           is_equally_deep subtree ~depth:(depth - 1)))
  ;;

  let rec num_leaves tree =
    match tree with
    | Idx _ -> 1
    | Subtree subtrees ->
      List.fold subtrees ~init:0 ~f:(fun acc subtree -> acc + num_leaves subtree)
  ;;

  let rec flatten tree =
    match tree with
    | Idx i -> [ i ]
    | Subtree subtrees -> List.concat (List.map subtrees ~f:flatten)
  ;;
end

let create_fanout_tree
  ?(reg_name_prefix = "fanout_reg")
  ?(reg_attributes = [])
  ~spec
  ~scope
  ~latency
  ~(tree : Tree.t)
  (root_element : Signal.t)
  =
  if not (Tree.is_equally_deep tree ~depth:(latency + 1))
  then
    raise_s
      [%message
        "Fan out tree specification is not equally deep of the given latency."
          (tree : Tree.t)
          (latency : int)];
  let num_leaves = Tree.num_leaves tree in
  if not
       (List.equal
          Int.equal
          (List.sort ~compare:Int.compare (Tree.flatten tree))
          (List.init num_leaves ~f:Fn.id))
  then
    raise_s
      [%message "The tree does not specify array indices correctly!" (tree : Tree.t)];
  let result = Array.init num_leaves ~f:(fun _ -> Signal.empty) in
  let rec aux signal (tree : Tree.t) ~is_at_root ~stage_idx =
    match tree with
    | Idx i -> result.(i) <- signal
    | Subtree subtrees ->
      let signal =
        if is_at_root
        then signal
        else (
          let reg_name = Printf.sprintf "%s_stage_%d" reg_name_prefix stage_idx in
          Signal.reg spec signal
          |> fun s ->
          List.fold ~init:s reg_attributes ~f:(fun s a -> Signal.add_attribute s a)
          |> fun s -> Scope.naming scope s reg_name)
      in
      List.iter subtrees ~f:(aux signal ~is_at_root:false ~stage_idx:(stage_idx + 1))
  in
  aux root_element tree ~is_at_root:true ~stage_idx:0;
  result
;;

let create_fanout_tree_intf
  (type element)
  (module Element : Interface.S_Of_signal with type Of_signal.t = element)
  ?reg_name_prefix
  ?reg_attributes
  ~spec
  ~scope
  ~latency
  ~(tree : Tree.t)
  (root_element : element)
  =
  create_fanout_tree
    ?reg_name_prefix
    ?reg_attributes
    ~spec
    ~scope
    ~latency
    ~tree
    (Element.Of_signal.pack root_element)
  |> Array.map ~f:Element.Of_signal.unpack
;;
