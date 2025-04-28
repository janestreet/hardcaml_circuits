open Hardcaml

module Tree : sig
  type t =
    | Idx of int
    | Subtree of t list
  [@@deriving sexp_of, variants]

  (** Returns a balanced tree with all nodes at the same depth, with [latency+1] number of
      Subtree nodes to each Idx node, and thus with the specified [latency] *)
  val create_balanced : leaves:int -> latency:int -> t

  (** Creates a balanced tree with the specified branching factor at every tree level. The
      total number of leaves will be the product of all branching factors, while the depth
      will be equal to the number of branching factors, and the latency will be one less
      than that.

      For example, branching factors [2; 4] will create a latency 1 tree like
      [{ Subtree [Subtree [Idx 0; Idx 1; Idx 2; Idx 3]; Subtree [Idx 4; Idx 5; Idx 6; Idx 7]] }] *)
  val create_from_branching_factors : branching_factors:int list -> t

  (** Checks that every path to a leaf node contains [depth] number of [Subtree] nodes. *)
  val is_equally_deep : depth:int -> t -> bool

  (** Counts the number of [Idx] nodes in the tree. *)
  val num_leaves : t -> int
end

(** Replicates a given signal with intermediate registers to create a fan-out tree. [tree]
    specifies how the indices in the output signal array should be grouped together in
    subtrees. [latency] is only used for sanity checking and should be equal to the tree
    depth minus 1.

    For example, [{ Subtree [Subtree [Idx 0; Idx 2]; Subtree [Idx 1; Idx 3]] }] should be
    called with latency = 1 and will create a tree as the following hardcaml code would
    [{ let tmp0 = reg input in let tmp1 = reg input in [ tmp0; tmp1; tmp0; tmp1 ] }] *)
val create_fanout_tree
  :  ?reg_name_prefix:string
  -> ?reg_attributes:Rtl_attribute.t list
  -> spec:Signal.Reg_spec.t
  -> scope:Scope.t
  -> latency:int
  -> tree:Tree.t
  -> Signal.t
  -> Signal.t array

val create_fanout_tree_intf
  :  (module Interface.S_Of_signal with type Of_signal.t = 'element)
  -> ?reg_name_prefix:string
  -> ?reg_attributes:Rtl_attribute.t list
  -> spec:Signal.Reg_spec.t
  -> scope:Scope.t
  -> latency:int
  -> tree:Tree.t
  -> 'element
  -> 'element array
