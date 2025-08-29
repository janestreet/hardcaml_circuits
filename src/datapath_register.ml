open! Base
open! Hardcaml
open Signal
module M_creates = Datapath_register_intf.M_creates

module Make (Data : Hardcaml.Interface.S) = struct
  module IO = struct
    type 'a t =
      { data : 'a Data.t
      ; valid : 'a
      ; ready : 'a
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; i : 'a IO.t [@rtlprefix "i_"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  let create_io ?(attributes = []) spec (i : _ IO.t) =
    let reg ~enable d =
      reg ~enable spec d |> fun init -> List.fold attributes ~init ~f:add_attribute
    in
    let wire0 () = Always.Variable.wire ~default:gnd () in
    let output_ready = i.ready in
    let temp_valid_reg = wire 1 in
    let output_valid_reg = wire 1 in
    let output_valid_next = wire0 () in
    let temp_valid_next = wire0 () in
    let store_input_to_output = wire0 () in
    let store_input_to_temp = wire0 () in
    let store_temp_to_output = wire0 () in
    let input_ready_early =
      output_ready |: (~:temp_valid_reg &: (~:output_valid_reg |: ~:(i.valid)))
    in
    let input_ready_reg = reg ~enable:vdd input_ready_early in
    output_valid_reg <-- reg ~enable:vdd output_valid_next.value;
    temp_valid_reg <-- reg ~enable:vdd temp_valid_next.value;
    Always.(
      compile
        [ (* transfer sink ready state to source *)
          output_valid_next <-- output_valid_reg
        ; temp_valid_next <-- temp_valid_reg
        ; store_input_to_output <--. 0
        ; store_input_to_temp <--. 0
        ; store_temp_to_output <--. 0
        ; if_
            input_ready_reg
            [ (* input is ready *)
              if_
                (output_ready |: ~:output_valid_reg)
                [ (* output is ready or currently not valid, transfer data to output *)
                  output_valid_next <-- i.valid
                ; store_input_to_output <--. 1
                ]
                [ (* output is not ready, store input in temp *)
                  temp_valid_next <-- i.valid
                ; store_input_to_temp <--. 1
                ]
            ]
          @@ elif
               output_ready
               [ (* input is not ready, but output is ready *)
                 output_valid_next <-- temp_valid_reg
               ; temp_valid_next <--. 0
               ; store_temp_to_output <--. 1
               ]
               []
        ]);
    let temp = Data.map i.data ~f:(reg ~enable:store_input_to_temp.value) in
    let output =
      Data.map (Data.Of_signal.mux2 store_input_to_output.value i.data temp) ~f:(fun x ->
        reg x ~enable:(store_input_to_output.value |: store_temp_to_output.value))
    in
    { IO.data = output; valid = output_valid_reg; ready = input_ready_reg }
  ;;

  let create ?attributes _scope (i : _ I.t) =
    let spec = Signal.Reg_spec.create () ~clock:i.clock ~clear:i.clear in
    create_io ?attributes spec i.i
  ;;

  let hierarchical ?instance ?attributes scope i =
    let module Scoped = Hierarchy.In_scope (I) (IO) in
    Scoped.hierarchical ~scope ~name:"datapath_reg" ?instance (create ?attributes) i
  ;;
end
