open! Base
open! Hardcaml
open! Signal

module Make (Clocking : Clocking.S) = struct
  type t =
    { input_clocking : Signal.t Clocking.t
    ; mutable clears : Signal.t list
    ; root_clear : Signal.t
    }

  let create input_clocking : t = { input_clocking; clears = []; root_clear = wire 1 }

  let node t : _ Clocking.t =
    let node = wire 1 in
    t.clears <- node :: t.clears;
    { clock = t.input_clocking.clock; clear = node }
  ;;

  let root t : _ Clocking.t = { clock = t.input_clocking.clock; clear = t.root_clear }

  let finalize ?(max_fanout = 500) ?latency ?clear_signal scope ~reg_name_prefix t =
    let leaves = List.length t.clears in
    let latency = Option.value latency ~default:(Int.ceil_log2 leaves) in
    let clear_signal =
      let%tydi { clock = _; clear } = t.input_clocking in
      Option.value_map clear_signal ~default:clear ~f:(fun f -> f clear)
    in
    t.root_clear <-- clear_signal;
    let tree =
      Fanout_tree.create_fanout_tree
        ~reg_attributes:
          [ Rtl_attribute.Vivado.Srl_style.register
          ; Rtl_attribute.Vivado.max_fanout max_fanout
          ]
        ~spec:(Clocking.to_spec_no_clear t.input_clocking)
        ~scope
        ~latency
        ~reg_name_prefix
        ~tree:(Fanout_tree.Tree.create_balanced ~latency ~leaves)
        clear_signal
    in
    List.iteri t.clears ~f:(fun idx node -> node <-- tree.(idx))
  ;;
end
