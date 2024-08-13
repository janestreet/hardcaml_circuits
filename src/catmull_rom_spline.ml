open Base

type spline_config =
  { num_control_points : int
  ; num_interpolation_points : int
  ; min_value : float
  ; control_point_step : float
  }

type interpolation_table = float array array [@@deriving sexp_of]
type control_point_table = float array [@@deriving sexp_of]

let t_vector t =
  let t2 = t *. t in
  let t3 = t *. t2 in
  Float.
    [| -t3 + (2. *. t2) - t
     ; (3. * t3) - (5. * t2) + 2.
     ; (-3. * t3) + (4. * t2) + t
     ; t3 - t2
    |]
;;

let interpolation_table config =
  Array.init config.num_interpolation_points ~f:(fun i ->
    let steps = config.num_interpolation_points in
    let t = Float.(of_int i / of_int steps) in
    t_vector t)
;;

let control_point_table config ~f =
  Array.init config.num_control_points ~f:(fun i ->
    let x = Float.(config.min_value + (of_int i * config.control_point_step)) in
    f x)
;;

let evaluate_at ~interpolation_table ~control_point_table ~k ~t =
  let t = interpolation_table.(t) in
  let p0 = control_point_table.(k + 0) in
  let p1 = control_point_table.(k + 1) in
  let p2 = control_point_table.(k + 2) in
  let p3 = control_point_table.(k + 3) in
  0.5 *. Float.((p0 * t.(0)) + (p1 * t.(1)) + (p2 * t.(2)) + (p3 * t.(3)))
;;

type precision_config =
  { interpolation_table_fixed_point : int
  ; control_point_fixed_point : int
  }

let control_point_integer_bits control_point_table =
  let max =
    Array.fold control_point_table ~init:0. ~f:(fun m v -> Float.max m (Float.abs v))
    |> Float.to_int
  in
  Hardcaml.Bits.num_bits_to_represent max + 1
;;

open Hardcaml

module Make (Comb : Comb.S) = struct
  module Fixed = Hardcaml_fixed_point.Signed (Comb)

  let create_multipliers precision_config ~control_point_table ~interpolation_table ~k ~t =
    let control_point_integer_bits = control_point_integer_bits control_point_table in
    let control_point_table =
      Array.map
        control_point_table
        ~f:
          (Fixed.of_float
             control_point_integer_bits
             precision_config.control_point_fixed_point)
      |> Array.to_list
    in
    let interpolation_table =
      List.init 4 ~f:(fun idx ->
        Array.map interpolation_table ~f:(fun v ->
          Fixed.of_float 3 precision_config.interpolation_table_fixed_point v.(idx))
        |> Array.to_list
        |> Fixed.mux t)
    in
    List.mapi interpolation_table ~f:(fun idx s ->
      let k = Comb.( +:. ) k idx in
      Fixed.(mux k control_point_table *: s))
  ;;

  let create_sum (t : Fixed.t list) =
    let s = Comb.tree ~arity:2 ~f:(Comb.reduce ~f:Fixed.( +: )) t in
    (* divide by two by incrementing the fixed point *)
    Fixed.create (Fixed.width_frac s + 1) (Fixed.signal s)
  ;;

  let create precision_config ~control_point_table ~interpolation_table ~k ~t =
    create_multipliers precision_config ~control_point_table ~interpolation_table ~k ~t
    |> create_sum
  ;;
end
