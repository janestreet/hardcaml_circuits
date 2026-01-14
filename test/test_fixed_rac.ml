open! Import

module Config = struct
  let mode = Rac.Mode.Fixed
  let accumulator_bits = 20
  let data_bits = 8
  let num_coefs = 4
  let rom_shift = 6
end

include Test_integer_rac.Make (Config)

let eval ~coefs ~data_in =
  Array.map2_exn coefs data_in ~f:( *. ) |> Array.reduce_exn ~f:( +. )
;;

let test_fixed ?print () ~coefs ~data_in ~coef_prec ~data_prec =
  let ref_result = eval ~coefs ~data_in in
  let rnd f = Float.to_int (if Float.compare f 0.0 < 0 then f -. 0.5 else f +. 0.5) in
  let scale_and_rnd prec f = rnd (f *. Float.of_int (1 lsl prec)) in
  let coefs = Array.map coefs ~f:(scale_and_rnd coef_prec) in
  let data_in = Array.map data_in ~f:(scale_and_rnd data_prec) in
  let result = test ?print () ~coefs ~data_in in
  let scale_result =
    Float.of_int (1 lsl Config.(coef_prec + 1 + data_prec + rom_shift - data_bits))
  in
  let float_result = Float.of_int result /. scale_result in
  print_s
    [%message
      ""
        ~precision:((coef_prec, data_prec) : int * int)
        (result : int)
        (ref_result : float)
        (float_result : float)]
;;

let%expect_test ("precisions" [@tags "runtime5-only"]) =
  let coefs = Array.init 4 ~f:(fun _ -> Random.float 1.) in
  let data_in = Array.init 4 ~f:(fun _ -> Random.float 1.) in
  print_s [%message "" (coefs : float array) (data_in : float array)];
  [%expect
    {|
    ((coefs (
       0.63535141231675607
       0.78255046724687982
       0.77208758810861389
       0.48715518185429985))
     (data_in (
       0.55401752229333867
       0.088701617893937754
       0.68236915472644311
       0.0278013320652886)))
    |}];
  test_fixed () ~coefs ~data_in ~coef_prec:3 ~data_prec:3;
  [%expect
    {|
    ((precision (3 3))
     (result       28)
     (ref_result   0.96180162561626825)
     (float_result 0.875))
    |}];
  test_fixed () ~coefs ~data_in ~coef_prec:5 ~data_prec:5;
  [%expect
    {|
    ((precision (5 5))
     (result       500)
     (ref_result   0.96180162561626825)
     (float_result 0.9765625))
    |}];
  test_fixed () ~coefs ~data_in ~coef_prec:7 ~data_prec:7;
  [%expect
    {|
    ((precision (7 7))
     (result       7856)
     (ref_result   0.96180162561626825)
     (float_result 0.958984375))
    |}];
  test_fixed () ~coefs ~data_in ~coef_prec:3 ~data_prec:5;
  [%expect
    {|
    ((precision (3 5))
     (result       122)
     (ref_result   0.96180162561626825)
     (float_result 0.953125))
    |}];
  test_fixed () ~coefs ~data_in ~coef_prec:5 ~data_prec:3;
  [%expect
    {|
    ((precision (5 3))
     (result       115)
     (ref_result   0.96180162561626825)
     (float_result 0.8984375))
    |}]
;;

let%expect_test ("signed" [@tags "runtime5-only"]) =
  let coefs = Array.init 4 ~f:(fun _ -> 2. *. (Random.float 1. -. 0.5)) in
  let data_in = Array.init 4 ~f:(fun _ -> 2. *. (Random.float 1. -. 0.5)) in
  print_s [%message "" (coefs : float array) (data_in : float array)];
  [%expect
    {|
    ((coefs (
       0.27070282463351214
       0.56510093449375964
       0.54417517621722777
       -0.025689636291400308))
     (data_in (
       0.10803504458667734
       -0.82259676421212446
       0.36473830945288621
       -0.94439733586942276)))
    |}];
  test_fixed () ~coefs ~data_in ~coef_prec:6 ~data_prec:6;
  [%expect
    {|
    ((precision (6 6))
     (result       -432)
     (ref_result   -0.21286205054604246)
     (float_result -0.2109375))
    |}]
;;

let%expect_test ("utilization" [@tags "runtime5-only"]) =
  let utilization =
    Circuit.create_exn
      ~name:"rac"
      (Rac.create ~coefs:(Array.init 4 ~f:(Fn.const (Bits.one 8))))
    |> Circuit_utilization.create
  in
  print_s [%message (utilization : Circuit_utilization.t)];
  [%expect
    {|
    (utilization (
      (name rac)
      (adders (
        (count             1)
        (total_bits        20)
        (max_instance_bits 20)))
      (subtractors (
        (count             1)
        (total_bits        20)
        (max_instance_bits 20)))
      (multiplexers (
        (count      7)
        (total_bits 208)
        (multiplexers (
          ((number_of_data_elements 2)
           (max_instance_bits       40)
           (total_bits              144)
           (count                   6))
          ((number_of_data_elements 16)
           (max_instance_bits       16)
           (total_bits              64)
           (count                   1))))))
      (registers     ((count 5)  (total_bits 52)))
      (constants     ((count 27) (total_bits 146)))
      (wires         ((count 24) (total_bits 146)))
      (concatenation ((count 12) (total_bits 126)))
      (part_selects  ((count 12) (total_bits 67)))))
    |}]
;;
