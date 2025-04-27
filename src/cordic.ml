open Base
open Hardcaml
open Signal
include Cordic_intf

module System = struct
  include Cordic_reference.System

  let const = function
    | Circular -> "00"
    | Hyperbolic -> "01"
    | Linear -> "10"
  ;;

  let to_signal t = of_string (const t)
end

module Mode = struct
  include Cordic_reference.Mode

  let const = function
    | Rotation -> "00"
    | Vectoring -> "01"
    | Inverse -> "11"
  ;;

  let to_signal t = of_string (const t)
end

module Make (Fixnum_spec : Fixnum.Spec) = struct
  module Fixnum = Fixnum.Make (Fixnum_spec)

  module Make_unrolled (B : Comb.S) = struct
    let iter = Cordic_reference.iter

    let atan iterations =
      Array.init iterations ~f:(fun i -> Fixnum.of_float Float.(atan (2. **. -of_int i)))
    ;;

    let atanh iterations =
      iter ~iterations ~init:[] ~f:(fun ~i:_ ~ih l ->
        Fixnum.of_float Float.(atanh (2. **. -of_int ih)) :: l)
      |> Array.of_list_rev
    ;;

    let t iterations = Array.init iterations ~f:(fun i -> Fixnum.pow2 (-i))

    let step ~x ~xsft ~y ~ysft ~z ~d ~m ~e =
      let open B in
      let x' = mux2 (msb m) x (mux2 (lsb m ^: d) (x +: ysft) (x -: ysft)) in
      let y' = mux2 d (y -: xsft) (y +: xsft) in
      let z' = mux2 d (z +: e) (z -: e) in
      x', y', z'
    ;;

    let cordic ?(pipe = Fn.id) ~system ~mode ~iterations ~c ~x ~y ~z () =
      let open B in
      assert (width x = width y);
      assert (width x = width z);
      let m = system in
      let e = [ atan iterations; atanh iterations; t iterations ] in
      let is_hyper = system ==: B.of_bit_string (System.const Hyperbolic) in
      iter ~iterations ~init:(x, y, z) ~f:(fun ~i ~ih (x, y, z) ->
        let e =
          mux system (List.map e ~f:(fun e -> B.of_bit_string (e.(i) |> Fixnum.constb)))
        in
        let d = mux mode [ z <+. 0; y >=+. 0; y >=+ c ] in
        let xsft = mux2 is_hyper (sra x ~by:ih) (sra x ~by:i) in
        let ysft = mux2 is_hyper (sra y ~by:ih) (sra y ~by:i) in
        let x, y, z = step ~x ~xsft ~y ~ysft ~z ~d ~m ~e in
        pipe x, pipe y, pipe z)
    ;;
  end

  module Iterative = struct
    module C = Make_unrolled (Signal)

    let hyper_iter ~reg_spec ~enable ~ld ~system ~iter =
      let is_hyper = system ==: of_bit_string (System.const Hyperbolic) in
      let iterh = wire (width iter) -- "iterh" in
      let k = wire (width iter + 2) -- "k" in
      let repeat = (k ==: zero 2 @: iterh) -- "repeated_step" in
      let upd v ~init t f =
        v
        <-- reg
              reg_spec
              ~enable
              (mux2 ld (of_int_trunc ~width:(width t) init) (mux2 repeat t f))
      in
      upd k ~init:4 (k +: sll k ~by:1 +:. 1) k;
      upd iterh ~init:1 iterh (iterh +:. 1);
      mux2 is_hyper iterh iter -- "iter_sel"
    ;;

    let cordic ~reg_spec ~enable ~ld ~system ~mode ~iterations ~c ~x ~y ~z =
      let wi = num_bits_to_represent (iterations - 1) in
      let iter =
        reg_fb reg_spec ~enable ~width:wi ~f:(fun d -> mux2 ld (zero wi) (d +:. 1))
        -- "iter"
      in
      let table_lookup table =
        mux
          iter
          (Array.to_list
             (Array.map table ~f:(fun c -> of_bit_string (c |> Fixnum.constb))))
      in
      let atan = table_lookup (C.atan iterations) in
      let atanh = table_lookup (C.atanh iterations) in
      let t = table_lookup (C.t iterations) in
      let xw, yw, zw = wire Fixnum.width, wire Fixnum.width, wire Fixnum.width in
      let m = system in
      let e = mux system [ atan; atanh; t ] -- "e" in
      let d = mux mode [ zw <+. 0; yw >=+. 0; yw >=+ c ] in
      let iter = hyper_iter ~reg_spec ~enable ~ld ~system ~iter in
      let xsft = log_shift ~f:sra xw ~by:iter in
      let ysft = log_shift ~f:sra yw ~by:iter in
      let xs, ys, zs = C.step ~x:xw ~xsft ~y:yw ~ysft ~z:zw ~d ~m ~e in
      xw <-- reg reg_spec ~enable (mux2 ld x xs);
      yw <-- reg reg_spec ~enable (mux2 ld y ys);
      zw <-- reg reg_spec ~enable (mux2 ld z zs);
      xw, yw, zw
    ;;
  end

  module Unrolled = Make_unrolled (Signal)

  module I = struct
    type 'a t =
      { clk : 'a
      ; clr : 'a
      ; enable : 'a [@bits 1]
      ; ld : 'a [@bits 1] (* load [x], [y], [z], and initialize state *)
      ; system : 'a [@bits 2]
      ; mode : 'a [@bits 2]
      ; c : 'a [@bits Fixnum.width]
      ; x : 'a [@bits Fixnum.width]
      ; y : 'a [@bits Fixnum.width]
      ; z : 'a [@bits Fixnum.width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { xo : 'a [@bits Fixnum.width]
      ; yo : 'a [@bits Fixnum.width]
      ; zo : 'a [@bits Fixnum.width]
      }
    [@@deriving hardcaml]
  end

  let create (config : Config.t) (i : Signal.t I.t) =
    let reg_spec = Reg_spec.create () ~clock:i.clk ~clear:i.clr in
    let iterations = config.iterations in
    let xo, yo, zo =
      let system, mode = i.system, i.mode in
      let x, y, z, c = i.x, i.y, i.z, i.c in
      match config.architecture with
      | Combinational -> Unrolled.cordic ~system ~mode ~iterations ~c ~x ~y ~z ()
      | Pipelined ->
        Unrolled.cordic
          ~pipe:(reg reg_spec ~enable:i.enable)
          ~system
          ~mode
          ~iterations
          ~c
          ~x
          ~y
          ~z
          ()
      | Iterative ->
        Iterative.cordic
          ~reg_spec
          ~enable:i.enable
          ~ld:i.ld
          ~system
          ~mode
          ~iterations
          ~c
          ~x
          ~y
          ~z
    in
    { O.xo; yo; zo }
  ;;
end
