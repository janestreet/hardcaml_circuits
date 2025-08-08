open Core
open Hardcaml

let create_circuit
  (type params)
  scope
  (module Component : Component_intf.S with type Params.t = params)
  (params : params)
  =
  let module I_data = struct
    module Pre = struct
      include Component.Input

      let port_names_and_widths = Component.input_port_names_and_width params
    end

    include Pre
    include Hardcaml.Interface.Make (Pre)
  end
  in
  let module I = struct
    type 'a t =
      { clock : 'a
      ; i_data : 'a I_data.t [@rtlprefix "i_data_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end
  in
  let module O = struct
    module Pre = struct
      include Component.Output

      let port_names_and_widths = Component.output_port_names_and_width params
    end

    include Pre
    include Hardcaml.Interface.Make (Pre)
  end
  in
  let create scope (input : _ I.t) =
    let spec_no_clear = Signal.Reg_spec.create ~clock:input.clock () in
    let reg x = Signal.reg spec_no_clear ~enable:Signal.vdd x in
    Component.create ~params ~scope ~clock:input.clock (I_data.map ~f:reg input.i_data)
    |> O.map ~f:reg
  in
  let name = Component.name ^ "__" ^ Component.Params.name params in
  let module Circuit = Hardcaml.Circuit.With_interface (I) (O) in
  Circuit.create_exn ~name (create scope)
;;

let name_exn signal =
  match Signal.names signal with
  | [ hd ] -> hd
  | _ -> assert false
;;

let circuit_data_inputs circuit =
  List.filter (Circuit.inputs circuit) ~f:(fun input ->
    not (String.equal (name_exn input) "clock"))
;;

let create_mega_circuit scope (circuits : Circuit.t list) =
  let db = Scope.circuit_database scope in
  let circuit_names =
    List.map circuits ~f:(fun circuit -> Circuit_database.insert db circuit)
  in
  let clock = Signal.input "clock" 1 in
  let data_inputs =
    let i = ref (-1) in
    List.map circuits ~f:(fun circuit ->
      List.map (circuit_data_inputs circuit) ~f:(fun inp ->
        Int.incr i;
        Signal.input (sprintf "data_in_%d" !i) (Signal.width inp)))
  in
  let outputs =
    let i = ref 0 in
    List.map3_exn
      circuits
      circuit_names
      data_inputs
      ~f:(fun circuit circuit_name data_inputs ->
        let circuit_data_input_names =
          List.map ~f:name_exn (circuit_data_inputs circuit)
        in
        let inputs =
          let clock_input = "clock", clock in
          let data_inputs =
            List.map2_exn circuit_data_input_names data_inputs ~f:(fun a b -> a, b)
          in
          clock_input :: data_inputs
        in
        let outputs =
          List.map (Circuit.outputs circuit) ~f:(fun s -> name_exn s, Signal.width s)
        in
        let output_names = List.map ~f:fst outputs in
        let inst = Instantiation.create ~name:circuit_name ~inputs ~outputs () in
        List.map output_names ~f:(fun name ->
          Int.incr i;
          Signal.output (sprintf "data_out_%d" !i) (Instantiation.output inst name)))
    |> List.concat
  in
  Circuit.create_exn ~name:"top" outputs
;;

let command_for_single =
  List.map Synth_targets.targets ~f:(fun component ->
    let (module Component : Component_intf.S) = component in
    let command =
      Async.Command.async
        ~summary:(Printf.sprintf "Single-component synthesis for %s" Component.name)
        [%map_open.Command
          let synth_flags = Hardcaml_xilinx_reports.Command.Command_flags.flags ()
          and params = Component.Params.flags in
          fun () ->
            Hardcaml_xilinx_reports.Command.run_circuit
              ~sort_by_name:true
              ~flags:synth_flags
              (fun scope -> create_circuit scope (module Component) params)]
    in
    let name = Component.name |> String.substr_replace_all ~pattern:"_" ~with_:"-" in
    name, command)
  |> Command.group ~summary:"Single component synthesis report"
;;

let command_for_all =
  let circuit scope =
    List.concat_map Synth_targets.targets ~f:(fun component ->
      let (module Component : Component_intf.S) = component in
      List.map Component.params ~f:(fun param ->
        create_circuit scope (module Component) param))
    |> create_mega_circuit scope
  in
  Hardcaml_xilinx_reports.Command.command_circuit ~sort_by_name:true circuit
;;

let command_old =
  Command.group
    ~summary:"Old-style non generic synthesis targets"
    [ "arbiters", Arbiters_old.command ]
;;

let command =
  Command.group
    ~summary:"synthesis reports commander"
    [ "all", command_for_all; "old", command_old; "single", command_for_single ]
;;
