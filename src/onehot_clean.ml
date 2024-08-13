open! Base
open! Hardcaml

type 'a t =
  { any_bit_set : 'a
  ; data : 'a
  }
[@@deriving hardcaml]

let scan_from_msb (type t) (module Bits : Comb.S with type t = t) (x : t) =
  let open Bits in
  let rec f x =
    if width x = 1
    then x, x
    else (
      let top, bot = split_in_half_msb x in
      let c1, top = f top in
      let c2, bot = f bot in
      c1 |: c2, top @: (bot &: sresize ~:c1 ~width:(width bot)))
  in
  let any_bit_set, data = f x in
  { any_bit_set; data }
;;

let scan_from_lsb (type t) (module Bits : Comb.S with type t = t) (x : t) =
  let open Bits in
  let rec f x =
    if width x = 1
    then x, x
    else (
      let top, bot = split_in_half_msb x in
      let c1, top = f top in
      let c2, bot = f bot in
      c1 |: c2, (top &: sresize ~:c2 ~width:(width top)) @: bot)
  in
  let any_bit_set, data = f x in
  { any_bit_set; data }
;;
