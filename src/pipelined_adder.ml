open Base
open Hardcaml
open Signal

(* Creates an architecture that looks like

   {v
   |  reg  |  reg  |   +  |
   |  reg  |   +   |  reg |
   |   +   |  reg  |  reg |
   v}

   Each adder is of size [part_width+1] and sums 3 operands (the third being a carry in
   bit). The adders are registered in the above diagram.

   The architecture requires a lot of registers for pipelining. Not providing clear should
   help to pack the register pipelines into SRLs.
*)
let create ~part_width ~clock ?clear ?(c_in = gnd) a b =
  assert (width a = width b);
  assert (width c_in = 1);
  let a = split_lsb ~exact:false ~part_width a in
  let b = split_lsb ~exact:false ~part_width b in
  let reg = reg (Reg_spec.create ~clock ?clear ()) ~enable:vdd in
  let regs = List.map ~f:reg in
  let rec f prev a b carry =
    match a, b with
    | [], [] -> prev
    | a :: at, b :: bt ->
      (* assuming that the synthesizer will perform the carry addition for 'free' *)
      let c = reg (ue a +: ue b +: uresize carry ~width:(width a + 1)) in
      f (lsbs c :: regs prev) (regs at) (regs bt) (msb c)
    | _ -> raise_s [%message "pipelined adder arguments are not the same width"]
  in
  f [] a b c_in |> concat_msb
;;

module Short_latency = struct
  type 'a sum =
    { c_out : 'a
    ; sum : 'a
    }
  [@@deriving sexp_of]

  type 'a sums =
    { sum0 : 'a sum
    ; sum1 : 'a sum
    }
  [@@deriving sexp_of]

  let partial_sums
    (type a)
    (module Comb : Comb.S with type t = a)
    ~part_width
    (a : a)
    (b : a)
    =
    let open Comb in
    let build_sums a b =
      let sum0 = Unsigned.(a +: b) in
      let sum1 = Unsigned.(a +: b) +:. 1 in
      let sum x = { c_out = msb x; sum = lsbs x } in
      { sum0 = sum sum0; sum1 = sum sum1 }
    in
    let a = split_lsb ~exact:false ~part_width a in
    let b = split_lsb ~exact:false ~part_width b in
    List.map2_exn a b ~f:build_sums
  ;;

  (* This is the core of the architecture. It takes the carry outs for [a+b] and [a+b+1]
     for each sub-part of the adder, and exposes the internal carry out as part of the
     sum. See the paper for more details. *)
  let carry_ahead (type a) (module Comb : Comb.S with type t = a) partial_sums =
    let open Comb in
    let s0 =
      List.map partial_sums ~f:(fun sums -> gnd @: sums.sum0.c_out) |> concat_lsb
    in
    let s1 =
      List.map partial_sums ~f:(fun sums -> vdd @: sums.sum1.c_out) |> concat_lsb
    in
    let cac_sum = s0 +: s1 in
    List.mapi partial_sums ~f:(fun i partial_sum ->
      if i = 0 then ~:(partial_sum.sum0.c_out) else cac_sum.:((i * 2) + 1))
  ;;

  let final_sums
    (type a)
    (module Comb : Comb.S with type t = a)
    (partial_sums : a sums list)
    (cac : a list)
    (c_in : a)
    =
    let open Comb in
    let rec f partial_sums cac c_in =
      match partial_sums, cac with
      | [], [] -> []
      | p :: pt, c :: ct ->
        (p.sum0.sum +: uresize c_in ~width:(width p.sum0.sum)) :: f pt ct ~:c
      | _ -> assert false
    in
    f partial_sums cac c_in
  ;;

  let comb (type a) (module Comb : Comb.S with type t = a) ~part_width (a : a) (b : a) =
    let partial_sums = partial_sums (module Comb) ~part_width a b in
    let cac = carry_ahead (module Comb) partial_sums in
    final_sums (module Comb) partial_sums cac Comb.gnd |> Comb.concat_lsb
  ;;

  let partial_sum_reg spec sum =
    let reg = reg spec ~enable:vdd in
    { sum0 = { c_out = reg sum.sum0.c_out; sum = reg sum.sum0.sum }
    ; sum1 = { c_out = reg sum.sum1.c_out; sum = reg sum.sum1.sum }
    }
  ;;

  let create ~part_width ~clock ?clear a b =
    let spec = Reg_spec.create ~clock ?clear () in
    let partial_sums = partial_sums (module Signal) ~part_width a b in
    let partial_sums = List.map partial_sums ~f:(partial_sum_reg spec) in
    let cac = carry_ahead (module Signal) partial_sums in
    let cac = List.map cac ~f:(reg spec ~enable:vdd) in
    let partial_sums = List.map partial_sums ~f:(partial_sum_reg spec) in
    final_sums (module Signal) partial_sums cac Signal.gnd |> Signal.concat_lsb
  ;;
end
