open Core
open Hardcaml_circuits.Catmull_rom_spline

let config ~log_control_points ~log_interpolation_points ~range:(minf, maxf) =
  let step = Float.((maxf - minf) / of_int (1 lsl log_control_points)) in
  { num_control_points = (1 lsl log_control_points) + 3
  ; num_interpolation_points = 1 lsl log_interpolation_points
  ; min_value = Float.(minf - step)
  ; control_point_step = step
  }
;;

let expected config ~f ~minf ~k ~t =
  let k = Float.(of_int k * config.control_point_step) in
  let t =
    Float.(config.control_point_step * of_int t / of_int config.num_interpolation_points)
  in
  f Float.(minf + k + t)
;;

let test
  ?(verbose = false)
  ~log_control_points
  ~log_interpolation_points
  ~range:(minf, maxf)
  f
  =
  let config = config ~log_control_points ~log_interpolation_points ~range:(minf, maxf) in
  let interpolation_table = interpolation_table config in
  let control_point_table = control_point_table config ~f in
  let max_error = ref 0. in
  let total_error = ref 0. in
  let count = ref 0 in
  for k = 0 to (1 lsl log_control_points) - 1 do
    for t = 0 to (1 lsl log_interpolation_points) - 1 do
      let catmull_rom = evaluate_at ~interpolation_table ~control_point_table ~k ~t in
      let expected = expected config ~f ~minf ~k ~t in
      let e = Float.(abs (catmull_rom - expected)) in
      total_error := !total_error +. e;
      if Float.(e > !max_error) then max_error := e;
      if verbose
      then Stdio.print_s [%message (catmull_rom : float) (expected : float) (e : float)];
      Int.incr count
    done
  done;
  let avg_error = Float.(!total_error / of_int !count) in
  printf "max_error = %.7e avg_error = %.7e\n" !max_error avg_error
;;

let%expect_test "sin in various ranges and precisions" =
  let f x = Float.(sin (x / pi)) in
  test ~log_control_points:2 ~log_interpolation_points:4 ~range:(0., 3.) f;
  [%expect {| max_error = 2.1746194e-04 avg_error = 1.2070261e-04 |}];
  test ~log_control_points:2 ~log_interpolation_points:8 ~range:(0., 1.) f;
  [%expect {| max_error = 8.0901614e-06 avg_error = 5.1570944e-06 |}];
  test ~log_control_points:3 ~log_interpolation_points:2 ~range:(0., 4.) f;
  [%expect {| max_error = 6.3513934e-05 avg_error = 2.5651782e-05 |}];
  test ~log_control_points:4 ~log_interpolation_points:6 ~range:(0., 3.) f;
  [%expect {| max_error = 3.4083087e-06 avg_error = 1.8925391e-06 |}]
;;

let%expect_test "tanh, [0..4]" =
  let f x = Float.(tanh x) in
  test ~verbose:false ~log_control_points:8 ~log_interpolation_points:7 ~range:(0., 4.) f;
  [%expect {| max_error = 1.2239537e-07 avg_error = 1.5308288e-08 |}];
  test ~verbose:false ~log_control_points:6 ~log_interpolation_points:6 ~range:(0., 4.) f;
  [%expect {| max_error = 7.8662356e-06 avg_error = 1.0069331e-06 |}];
  test ~verbose:false ~log_control_points:2 ~log_interpolation_points:4 ~range:(0., 4.) f;
  [%expect {| max_error = 4.8271308e-02 avg_error = 9.3477207e-03 |}]
;;

open Hardcaml

let testhw
  ?(verbose = false)
  ~log_control_points
  ~log_interpolation_points
  ~precision_config
  ~range:(minf, maxf)
  f
  =
  let module Hw = Make (Bits) in
  let config = config ~log_control_points ~log_interpolation_points ~range:(minf, maxf) in
  let interpolation_table = interpolation_table config in
  let control_point_table = control_point_table config ~f in
  let max_error = ref 0. in
  let total_error = ref 0. in
  let count = ref 0 in
  for k = 0 to (1 lsl log_control_points) - 1 do
    for t = 0 to (1 lsl log_interpolation_points) - 1 do
      let catmull_rom =
        Hw.create
          precision_config
          ~control_point_table
          ~interpolation_table
          ~k:
            (Bits.of_int_trunc ~width:(Bits.address_bits_for config.num_control_points) k)
          ~t:
            (Bits.of_int_trunc
               ~width:(Bits.address_bits_for config.num_interpolation_points)
               t)
        |> Hw.Fixed.to_float
      in
      let expected = expected config ~f ~minf ~k ~t in
      let e = Float.(abs (catmull_rom - expected)) in
      total_error := !total_error +. e;
      if Float.(e > !max_error) then max_error := e;
      if verbose
      then Stdio.print_s [%message (catmull_rom : float) (expected : float) (e : float)];
      Int.incr count
    done
  done;
  let avg_error = Float.(!total_error / of_int !count) in
  Stdio.print_s [%message (!max_error : float) (avg_error : float)]
;;

let%expect_test "hardware" =
  let f x = Float.(tanh x) in
  let testhw =
    testhw ~log_control_points:6 ~log_interpolation_points:6 ~range:(0., 4.) f
  in
  List.iter [ 5; 10; 15; 20 ] ~f:(fun fixed_point ->
    testhw
      ~precision_config:
        { interpolation_table_fixed_point = fixed_point
        ; control_point_fixed_point = fixed_point
        });
  [%expect
    {|
    ((!max_error 0.0464840326959437) (avg_error 0.020945182987315559))
    ((!max_error 0.0014126140238234219) (avg_error 0.00054686427072402034))
    ((!max_error 3.6236607839440493E-05) (avg_error 1.6177189263494835E-05))
    ((!max_error 8.6243211220859983E-06) (avg_error 1.2299979479452502E-06))
    |}]
;;
