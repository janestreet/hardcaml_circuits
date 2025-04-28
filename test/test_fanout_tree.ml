open Core
open Hardcaml
open Hardcaml_circuits.Fanout_tree

let print tree = Tree.sexp_of_t tree |> Sexp_pretty.sexp_to_string |> print_string

let%expect_test "test 1 layer balanced tree" =
  print (Tree.create_balanced ~leaves:1 ~latency:0);
  [%expect {| (Subtree ((Idx 0))) |}];
  print (Tree.create_balanced ~leaves:4 ~latency:0);
  [%expect
    {|
    (Subtree (
      (Idx 0)
      (Idx 1)
      (Idx 2)
      (Idx 3)))
    |}]
;;

let%expect_test "test 2 layer balanced tree" =
  print (Tree.create_balanced ~leaves:1 ~latency:1);
  [%expect {| (Subtree ((Subtree ((Idx 0))))) |}];
  print (Tree.create_balanced ~leaves:3 ~latency:1);
  [%expect
    {|
    (Subtree (
      (Subtree ((Idx 0)))
      (Subtree ((Idx 1)))
      (Subtree ((Idx 2)))))
    |}];
  print (Tree.create_balanced ~leaves:4 ~latency:1);
  [%expect
    {|
    (Subtree (
      (Subtree ((Idx 0) (Idx 1)))
      (Subtree ((Idx 2) (Idx 3)))))
    |}];
  print (Tree.create_balanced ~leaves:7 ~latency:1);
  [%expect
    {|
    (Subtree (
      (Subtree ((Idx 0) (Idx 1)))
      (Subtree ((Idx 2) (Idx 3)))
      (Subtree (
        (Idx 4)
        (Idx 5)
        (Idx 6)))))
    |}];
  print (Tree.create_balanced ~leaves:8 ~latency:1);
  [%expect
    {|
    (Subtree (
      (Subtree ((Idx 0) (Idx 1)))
      (Subtree ((Idx 2) (Idx 3)))
      (Subtree ((Idx 4) (Idx 5)))
      (Subtree ((Idx 6) (Idx 7)))))
    |}]
;;

let%expect_test "test 3 layer balanced tree" =
  print (Tree.create_balanced ~leaves:1 ~latency:2);
  [%expect {| (Subtree ((Subtree ((Subtree ((Idx 0))))))) |}];
  print (Tree.create_balanced ~leaves:3 ~latency:2);
  [%expect
    {|
    (Subtree (
      (Subtree ((Subtree ((Idx 0)))))
      (Subtree ((Subtree ((Idx 1)))))
      (Subtree ((Subtree ((Idx 2)))))))
    |}];
  print (Tree.create_balanced ~leaves:4 ~latency:2);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree ((Idx 0)))
        (Subtree ((Idx 1)))))
      (Subtree (
        (Subtree ((Idx 2)))
        (Subtree ((Idx 3)))))))
    |}];
  print (Tree.create_balanced ~leaves:6 ~latency:2);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree ((Idx 0)))
        (Subtree ((Idx 1)))
        (Subtree ((Idx 2)))))
      (Subtree (
        (Subtree ((Idx 3)))
        (Subtree ((Idx 4)))
        (Subtree ((Idx 5)))))))
    |}];
  print (Tree.create_balanced ~leaves:7 ~latency:2);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree ((Idx 0)))
        (Subtree ((Idx 1)))
        (Subtree ((Idx 2)))))
      (Subtree (
        (Subtree ((Idx 3) (Idx 4)))
        (Subtree ((Idx 5) (Idx 6)))))))
    |}]
;;

let%expect_test "test on many leaves" =
  print (Tree.create_balanced ~leaves:30 ~latency:1);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Idx 0)
        (Idx 1)
        (Idx 2)
        (Idx 3)
        (Idx 4)))
      (Subtree (
        (Idx 5)
        (Idx 6)
        (Idx 7)
        (Idx 8)
        (Idx 9)))
      (Subtree (
        (Idx 10)
        (Idx 11)
        (Idx 12)
        (Idx 13)
        (Idx 14)))
      (Subtree (
        (Idx 15)
        (Idx 16)
        (Idx 17)
        (Idx 18)
        (Idx 19)))
      (Subtree (
        (Idx 20)
        (Idx 21)
        (Idx 22)
        (Idx 23)
        (Idx 24)))
      (Subtree (
        (Idx 25)
        (Idx 26)
        (Idx 27)
        (Idx 28)
        (Idx 29)))))
    |}];
  print (Tree.create_balanced ~leaves:30 ~latency:2);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree ((Idx 0) (Idx 1)))
        (Subtree ((Idx 2) (Idx 3)))
        (Subtree (
          (Idx 4)
          (Idx 5)
          (Idx 6)))))
      (Subtree (
        (Subtree ((Idx 7) (Idx 8)))
        (Subtree ((Idx 9) (Idx 10)))
        (Subtree (
          (Idx 11)
          (Idx 12)
          (Idx 13)))))
      (Subtree (
        (Subtree ((Idx 14) (Idx 15)))
        (Subtree ((Idx 16) (Idx 17)))
        (Subtree (
          (Idx 18)
          (Idx 19)
          (Idx 20)))))
      (Subtree (
        (Subtree (
          (Idx 21)
          (Idx 22)
          (Idx 23)))
        (Subtree (
          (Idx 24)
          (Idx 25)
          (Idx 26)))
        (Subtree (
          (Idx 27)
          (Idx 28)
          (Idx 29)))))))
    |}];
  print (Tree.create_balanced ~leaves:30 ~latency:3);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree (
          (Subtree ((Idx 0)))
          (Subtree ((Idx 1)))
          (Subtree ((Idx 2)))))
        (Subtree (
          (Subtree ((Idx 3)))
          (Subtree ((Idx 4)))
          (Subtree ((Idx 5)))))
        (Subtree (
          (Subtree ((Idx 6) (Idx 7)))
          (Subtree ((Idx 8) (Idx 9)))))))
      (Subtree (
        (Subtree (
          (Subtree ((Idx 10)))
          (Subtree ((Idx 11)))
          (Subtree ((Idx 12)))))
        (Subtree (
          (Subtree ((Idx 13)))
          (Subtree ((Idx 14)))
          (Subtree ((Idx 15)))))
        (Subtree (
          (Subtree ((Idx 16) (Idx 17)))
          (Subtree ((Idx 18) (Idx 19)))))))
      (Subtree (
        (Subtree (
          (Subtree ((Idx 20)))
          (Subtree ((Idx 21)))
          (Subtree ((Idx 22)))))
        (Subtree (
          (Subtree ((Idx 23)))
          (Subtree ((Idx 24)))
          (Subtree ((Idx 25)))))
        (Subtree (
          (Subtree ((Idx 26) (Idx 27)))
          (Subtree ((Idx 28) (Idx 29)))))))))
    |}]
;;

let%expect_test "test tree from branching factors" =
  print (Tree.create_from_branching_factors ~branching_factors:[ 3 ]);
  [%expect
    {|
    (Subtree (
      (Idx 0)
      (Idx 1)
      (Idx 2)))
    |}];
  print (Tree.create_from_branching_factors ~branching_factors:[ 2; 3 ]);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Idx 0)
        (Idx 1)
        (Idx 2)))
      (Subtree (
        (Idx 3)
        (Idx 4)
        (Idx 5)))))
    |}];
  print (Tree.create_from_branching_factors ~branching_factors:[ 2; 3; 1 ]);
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree ((Idx 0)))
        (Subtree ((Idx 1)))
        (Subtree ((Idx 2)))))
      (Subtree (
        (Subtree ((Idx 3)))
        (Subtree ((Idx 4)))
        (Subtree ((Idx 5)))))))
    |}];
  print (Tree.create_from_branching_factors ~branching_factors:[ 1; 2; 3; 1 ]);
  [%expect
    {|
    (Subtree ((
      Subtree (
        (Subtree (
          (Subtree ((Idx 0)))
          (Subtree ((Idx 1)))
          (Subtree ((Idx 2)))))
        (Subtree (
          (Subtree ((Idx 3)))
          (Subtree ((Idx 4)))
          (Subtree ((Idx 5)))))))))
    |}]
;;

(* Define a scalar interface *)
module Example = struct
  include Types.Scalar (struct
      let port_name = "example"
      let port_width = 8
    end)
end

(* Create a fan-out tree and print it *)
let%expect_test "fan-out tree with custom tree" =
  let spec = Signal.Reg_spec.create ~clock:(Signal.input "clock" 1) () in
  let scope = Scope.create () in
  let root_element = Example.Of_signal.inputs () in
  let tree : Tree.t =
    Subtree
      [ Subtree [ Idx 0; Idx 1 ]; Subtree [ Idx 2 ]; Subtree [ Idx 3; Idx 4; Idx 5 ] ]
  in
  print tree;
  let latency = 1 in
  let result =
    create_fanout_tree_intf (module Example) ~scope ~spec ~tree ~latency root_element
  in
  let result =
    Array.mapi result ~f:(fun i s -> Signal.output ("o" ^ Int.to_string i) s)
  in
  print_s [%message (result : Signal.t array)];
  Circuit.create_exn ~name:"test" (Array.to_list result) |> Rtl.print Rtl.Language.Verilog;
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Idx 0)
        (Idx 1)))
      (Subtree ((Idx 2)))
      (Subtree (
        (Idx 3)
        (Idx 4)
        (Idx 5)))))
    (result
     ((wire (names (o0)) (width 8) (data_in fanout_reg_stage_1))
      (wire (names (o1)) (width 8) (data_in fanout_reg_stage_1))
      (wire (names (o2)) (width 8) (data_in fanout_reg_stage_1))
      (wire (names (o3)) (width 8) (data_in fanout_reg_stage_1))
      (wire (names (o4)) (width 8) (data_in fanout_reg_stage_1))
      (wire (names (o5)) (width 8) (data_in fanout_reg_stage_1))))
    module test (
        clock,
        example,
        o0,
        o1,
        o2,
        o3,
        o4,
        o5
    );

        input clock;
        input [7:0] example;
        output [7:0] o0;
        output [7:0] o1;
        output [7:0] o2;
        output [7:0] o3;
        output [7:0] o4;
        output [7:0] o5;

        reg [7:0] fanout_reg_stage_1;
        reg [7:0] fanout_reg_stage_1_1;
        reg [7:0] fanout_reg_stage_1_2;
        always @(posedge clock) begin
            fanout_reg_stage_1 <= example;
        end
        always @(posedge clock) begin
            fanout_reg_stage_1_1 <= example;
        end
        always @(posedge clock) begin
            fanout_reg_stage_1_2 <= example;
        end
        assign o0 = fanout_reg_stage_1_2;
        assign o1 = fanout_reg_stage_1_2;
        assign o2 = fanout_reg_stage_1_1;
        assign o3 = fanout_reg_stage_1;
        assign o4 = fanout_reg_stage_1;
        assign o5 = fanout_reg_stage_1;

    endmodule
    |}]
;;

let%expect_test "fan-out tree with balanced tree" =
  let spec = Signal.Reg_spec.create ~clock:(Signal.input "clock" 1) () in
  let scope = Scope.create () in
  let root_element = Example.Of_signal.inputs () in
  let tree = Tree.create_balanced ~leaves:9 ~latency:3 in
  print tree;
  let latency = 3 in
  let result =
    create_fanout_tree_intf (module Example) ~spec ~scope ~tree ~latency root_element
  in
  let result =
    Array.mapi result ~f:(fun i s -> Signal.output ("o" ^ Int.to_string i) s)
  in
  print_s [%message (result : Signal.t array)];
  Circuit.create_exn ~name:"test" (Array.to_list result) |> Rtl.print Rtl.Language.Verilog;
  [%expect
    {|
    (Subtree (
      (Subtree (
        (Subtree (
          (Subtree ((Idx 0)))
          (Subtree ((Idx 1)))))
        (Subtree (
          (Subtree ((Idx 2)))
          (Subtree ((Idx 3)))))))
      (Subtree (
        (Subtree (
          (Subtree ((Idx 4)))
          (Subtree ((Idx 5)))))
        (Subtree (
          (Subtree ((Idx 6)))
          (Subtree ((Idx 7)))
          (Subtree ((Idx 8)))))))))
    (result
     ((wire (names (o0)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o1)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o2)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o3)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o4)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o5)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o6)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o7)) (width 8) (data_in fanout_reg_stage_3))
      (wire (names (o8)) (width 8) (data_in fanout_reg_stage_3))))
    module test (
        clock,
        example,
        o0,
        o1,
        o2,
        o3,
        o4,
        o5,
        o6,
        o7,
        o8
    );

        input clock;
        input [7:0] example;
        output [7:0] o0;
        output [7:0] o1;
        output [7:0] o2;
        output [7:0] o3;
        output [7:0] o4;
        output [7:0] o5;
        output [7:0] o6;
        output [7:0] o7;
        output [7:0] o8;

        reg [7:0] fanout_reg_stage_3;
        reg [7:0] fanout_reg_stage_3_1;
        reg [7:0] fanout_reg_stage_2;
        reg [7:0] fanout_reg_stage_3_2;
        reg [7:0] fanout_reg_stage_3_3;
        reg [7:0] fanout_reg_stage_1;
        reg [7:0] fanout_reg_stage_2_1;
        reg [7:0] fanout_reg_stage_3_4;
        reg [7:0] fanout_reg_stage_3_5;
        reg [7:0] fanout_reg_stage_2_2;
        reg [7:0] fanout_reg_stage_3_6;
        reg [7:0] fanout_reg_stage_3_7;
        reg [7:0] fanout_reg_stage_1_1;
        reg [7:0] fanout_reg_stage_2_3;
        reg [7:0] fanout_reg_stage_3_8;
        always @(posedge clock) begin
            fanout_reg_stage_3 <= fanout_reg_stage_2;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_1 <= fanout_reg_stage_2;
        end
        always @(posedge clock) begin
            fanout_reg_stage_2 <= fanout_reg_stage_1;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_2 <= fanout_reg_stage_2;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_3 <= fanout_reg_stage_2_1;
        end
        always @(posedge clock) begin
            fanout_reg_stage_1 <= example;
        end
        always @(posedge clock) begin
            fanout_reg_stage_2_1 <= fanout_reg_stage_1;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_4 <= fanout_reg_stage_2_1;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_5 <= fanout_reg_stage_2_2;
        end
        always @(posedge clock) begin
            fanout_reg_stage_2_2 <= fanout_reg_stage_1_1;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_6 <= fanout_reg_stage_2_2;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_7 <= fanout_reg_stage_2_3;
        end
        always @(posedge clock) begin
            fanout_reg_stage_1_1 <= example;
        end
        always @(posedge clock) begin
            fanout_reg_stage_2_3 <= fanout_reg_stage_1_1;
        end
        always @(posedge clock) begin
            fanout_reg_stage_3_8 <= fanout_reg_stage_2_3;
        end
        assign o0 = fanout_reg_stage_3_8;
        assign o1 = fanout_reg_stage_3_7;
        assign o2 = fanout_reg_stage_3_6;
        assign o3 = fanout_reg_stage_3_5;
        assign o4 = fanout_reg_stage_3_4;
        assign o5 = fanout_reg_stage_3_3;
        assign o6 = fanout_reg_stage_3_2;
        assign o7 = fanout_reg_stage_3_1;
        assign o8 = fanout_reg_stage_3;

    endmodule
    |}]
;;
