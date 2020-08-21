open Base
open Hardcaml

let ceil_log ~base x =
  let rec loop acc = if x <= acc then 0 else 1 + loop (acc * base) in
  if x <= 0 then raise_s [%message "ceil_log expects a positive integer as argument"];
  loop 1
;;

let%expect_test "ceil log" =
  let test ~base x =
    Stdio.printf "ceil_log ~base:%d %d = %d\n" base x (ceil_log ~base x)
  in
  test ~base:32 1;
  [%expect {| ceil_log ~base:32 1 = 0 |}];
  test ~base:32 3;
  [%expect {| ceil_log ~base:32 3 = 1 |}];
  test ~base:32 32;
  [%expect {| ceil_log ~base:32 32 = 1 |}];
  test ~base:5 125;
  [%expect {| ceil_log ~base:5 125 = 3 |}];
  test ~base:5 126;
  [%expect {| ceil_log ~base:5 126 = 4 |}]
;;

let create ~f ~enable ~arity spec args =
  let enable =
    let pipeline_depth = ceil_log ~base:arity (List.length args) in
    Signal.pipeline ~n:pipeline_depth ~enable:Signal.vdd spec enable
  in
  { With_valid.valid = enable
  ; value =
      Signal.tree
        ~arity
        ~f:(fun l -> Signal.reg ~enable:Signal.vdd spec (Signal.reduce l ~f))
        args
  }
;;
