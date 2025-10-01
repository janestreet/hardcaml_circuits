open Base
open Hardcaml
open Signal

let create ~part_width ~clock ~clear ~(set : Signal.t With_valid.t) ~increment =
  assert (part_width >= width increment);
  let width = width set.value in
  let spec = Reg_spec.create ~clock ~clear () in
  let reg = reg spec ~enable:vdd in
  let set_value =
    uresize set.value ~width:(Int.round_up ~to_multiple_of:part_width width)
    |> split_lsb ~part_width
  in
  let rec f incr parts set_valid set_value =
    let create_part set_value =
      let w = wire part_width in
      let a = mux2 set_valid (gnd @: set_value) Unsigned.(w +: incr) in
      let lsbs = reg (lsbs a) in
      w <-- lsbs;
      let msb = reg (msb a) in
      msb, lsbs
    in
    match set_value with
    | [] -> (concat_msb parts).:[width - 1, 0]
    | set_value :: tl ->
      let carry, d = create_part set_value in
      f carry (d :: List.map parts ~f:reg) (reg set_valid) (List.map tl ~f:reg)
  in
  f increment [] set.valid set_value
;;
