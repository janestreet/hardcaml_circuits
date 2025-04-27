(** Catmull-rom spline interpolation.

    Given an arbitrary function [f] we can create an approximation of the function using
    splines for interpolation. The function is evaluated at some number of control points,
    then between each control point we can approximate it at some number of interpolation
    points.

    The spacing between control and interpolation points is arbitrary, though to be useful
    you are likely to want divide the intervals by powers of 2 so that the input value can
    index the control point and interpolation point tables by simple bit selection. The
    tests show how to configure the spline for this.

    For the hardware implemention, we also need to specify the fractional precision we
    want to use. We can experiment with the floating point version to find the minimum
    error for a specific configuration, then tweak the fixed points to get something
    suitable. *)

open Base
open Hardcaml

type spline_config =
  { num_control_points : int (** Number of control points to store. *)
  ; num_interpolation_points : int
  (** Number of interpolation points between control points. *)
  ; min_value : float (** Value of the first control point. *)
  ; control_point_step : float (** Step between each control point. *)
  }

type interpolation_table = float array array [@@deriving sexp_of]
type control_point_table = float array [@@deriving sexp_of]

val interpolation_table : spline_config -> interpolation_table
val control_point_table : spline_config -> f:(float -> float) -> control_point_table

val evaluate_at
  :  interpolation_table:interpolation_table
  -> control_point_table:control_point_table
  -> k:int
  -> t:int
  -> float

type precision_config =
  { interpolation_table_fixed_point : int
  (** Fixed point of interpolation table values (integer part if fixed to range in +/- 2). *)
  ; control_point_fixed_point : int
  (** Fixed point of control point values (integer part will be inferred from evaluating
      the given function). *)
  }

module Make (Comb : Comb.S) : sig
  module Fixed : module type of Hardcaml_fixed_point.Signed (Comb)

  (** Create 4 fixed point multipliers. *)
  val create_multipliers
    :  precision_config
    -> control_point_table:control_point_table
    -> interpolation_table:interpolation_table
    -> k:Comb.t
    -> t:Comb.t
    -> Fixed.t list

  (** Sum multiplication results and divide by 2. *)
  val create_sum : Fixed.t list -> Fixed.t

  (** [create_multipliers] followed by [create_sum] connected combinationally. *)
  val create
    :  precision_config
    -> control_point_table:control_point_table
    -> interpolation_table:interpolation_table
    -> k:Comb.t
    -> t:Comb.t
    -> Fixed.t
end
