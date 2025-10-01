open Base
open Hardcaml
open Hardcaml_circuits
open Hardcaml_waveterm
open Bits

module Data = Types.Value (struct
    let port_name = "data"
    let port_width = 16
  end)

include Datapath_register.Make (Data)
module Sim = Cyclesim.With_interface (I) (IO)

let create_sim ?(all_waves = false) () =
  let dut =
    create (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
  in
  let sim =
    let config =
      { (if all_waves then Cyclesim.Config.trace_all else Cyclesim.Config.default) with
        store_circuit = true
      }
    in
    Sim.create ~config dut
  in
  Waveform.create sim
;;

let run_test ~sim n =
  let input_queue = Queue.create () in
  let inputs : _ I.t = Cyclesim.inputs sim in
  let outputs : _ IO.t = Cyclesim.outputs sim in
  let rec run_cycle ?(data_index = 0) () =
    inputs.i.valid := Base.Random.bool () |> of_bool;
    inputs.i.ready := Base.Random.bool () |> of_bool;
    inputs.i.data := Bits.of_int_trunc ~width:16 data_index;
    let data_index =
      if to_bool (!(inputs.i.valid) &: !(outputs.ready)) && data_index < n
      then (
        Queue.enqueue input_queue !(inputs.i.data);
        data_index + 1)
      else data_index
    in
    if to_bool (!(outputs.valid) &: !(inputs.i.ready))
    then (
      match Queue.dequeue input_queue with
      | None -> raise_s [%message "No inputs in queue" [%here]]
      | Some expect -> [%test_result: t] !(outputs.data) ~expect);
    Cyclesim.cycle sim;
    if data_index = n && Queue.length input_queue = 0
    then ()
    else run_cycle ~data_index ()
  in
  run_cycle ()
;;

let save_waves = false

let%expect_test "test_datapath_register" =
  let waves, sim = create_sim ~all_waves:true () in
  let n = 1000 in
  Core.Exn.protect
    ~f:(fun () -> run_test ~sim n)
    ~finally:(fun () ->
      if save_waves
      then Waveform.Serialize.marshall waves "test_datapath_register.hardcamlwaveform")
;;
