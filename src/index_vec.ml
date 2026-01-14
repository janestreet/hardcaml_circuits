open Base
open Hardcaml
open Signal

module type Arg = Index_vec_intf.Arg
module type S = Index_vec_intf.S

module Make_tagged (Arg : Arg) = struct
  open Arg

  let log_vec_size = Int.ceil_log2 vec_size
  let insert = Vec.insert
  let remove = Vec.remove

  (* The data tracked in the vec. The index is the place in the external ram the bulk of
     data is supposed to be kept. The tag is an optional value which shifts along with the
     indexes - it is for special use cases such as an associated valid bit with an index.
  *)
  module Interface = struct
    type 'a t =
      { index : 'a [@bits log_vec_size]
      ; tag : 'a Arg.Tag.t
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  (* Construct the vec. The indexes need special values written when the table is reset.
     The tag reset value is up to the user. *)
  module Vec = Vec.Make (struct
      module Interface = Interface

      (* Initialize the indexes with [0,1,2,3,4, ...]. Tags are up to the user. *)
      let spec ~index =
        { Interface.index = of_int_trunc ~width:log_vec_size index
        ; tag = Arg.spec ~index
        }
      ;;
    end)

  type t =
    { vec : Vec.t
    ; length : Signal.t
    ; full : Signal.t
    ; empty : Signal.t
    ; insertion_index : Signal.t
    ; deletion_index : Signal.t
    }
  [@@deriving fields ~getters]

  type op =
    { slot : Signal.t
    ; op : Signal.t
    ; insertion_tag : Signal.t Arg.Tag.t option
    ; deletion_tag : Signal.t Arg.Tag.t option
    }

  let create ?index_next ~tag_next spec op =
    let insertion_index = Interface.Of_signal.wires () in
    let deletion_index = Interface.Of_signal.wires () in
    let vec =
      Vec.create
        spec
        ~vec_size
        ~next:(fun ~index d ->
          { index =
              Option.value_map index_next ~default:d.index ~f:(fun f -> f ~index d.index)
          ; tag = tag_next ~index d.tag
          })
        { slot = op.slot
        ; op = op.op
        ; insert_data = insertion_index
        ; delete_data = deletion_index
        }
    in
    (* The last element will be used for inserting into the vec. The currently addressed
       element will be popped from the vec and placed at the end. *)
    Interface.iter2
      insertion_index
      (let d = Vec.get vec ~index:(vec_size - 1) in
       { d with tag = Option.value ~default:d.tag op.insertion_tag })
      ~f:( <-- );
    Interface.iter2
      deletion_index
      (let d = Vec.read_mux ~index:op.slot vec in
       { d with tag = Option.value ~default:d.tag op.deletion_tag })
      ~f:( <-- );
    (* the following is entirely optional, but useful for debugging. Track the highest
       index inserted, and the size of the vec as items are deleted. It means somewhat
       less if the user arbitrarily writes to slots (which is fine, but those indexes are
       not tracked by this module). *)
    let length = wire (Int.ceil_log2 (vec_size + 1)) in
    let do_insert = op.op ==: insert (module Signal) in
    let do_remove = op.op ==: remove (module Signal) in
    length
    <-- reg_fb
          spec
          ~enable:(do_insert |: do_remove)
          ~width:(Int.ceil_log2 (vec_size + 1))
          ~f:(fun length ->
            let slot = uresize op.slot ~width:(width length) in
            let next = length +:. 1 in
            let prev = length -:. 1 in
            let max = of_int_trunc ~width:(width length) vec_size in
            let min = of_int_trunc ~width:(width length) 0 in
            let slot_is_empty = slot >=: length in
            let on_insert =
              mux2 (length ==: max) max @@ mux2 slot_is_empty (slot +:. 1) next
            in
            let on_delete = mux2 (length ==:. 0) min @@ mux2 slot_is_empty length prev in
            mux2 do_insert on_insert on_delete);
    let full = length ==:. vec_size in
    let empty = length ==:. 0 in
    { vec
    ; length
    ; full
    ; empty
    ; insertion_index = insertion_index.index
    ; deletion_index = deletion_index.index
    }
  ;;

  let indexes t =
    Array.init (Vec.vec_size t.vec) ~f:(fun index -> (Vec.get t.vec ~index).index)
  ;;

  let index ~at t = (Vec.read_mux ~index:at t.vec).index

  let tags t =
    Array.init (Vec.vec_size t.vec) ~f:(fun index -> (Vec.get t.vec ~index).tag)
  ;;

  let tag ~at t = (Vec.read_mux ~index:at t.vec).tag

  (* binding just for documentation purposes. *)
  let access_index = deletion_index
end

module Make (Arg : sig
    val vec_size : int
  end) =
struct
  module Tagged = Make_tagged (struct
      let vec_size = Arg.vec_size

      module Tag = Interface.Empty

      let spec ~index:_ = Interface.Empty.Empty
    end)

  include Tagged

  type op =
    { slot : Signal.t
    ; op : Signal.t
    }

  let create ?index_next spec op =
    Tagged.create
      ~tag_next:(fun ~index:_ -> Fn.id)
      ?index_next
      spec
      { Tagged.slot = op.slot; op = op.op; insertion_tag = None; deletion_tag = None }
  ;;
end
