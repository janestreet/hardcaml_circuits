open Base
open! Hardcaml

(* xapp052  2..168 *)
let taps =
  [| []
   ; []
   ; [ 2; 1 ]
   ; [ 3; 2 ]
   ; [ 4; 3 ]
   ; [ 5; 3 ]
   ; [ 6; 5 ]
   ; [ 7; 6 ]
   ; [ 8; 6; 5; 4 ]
   ; [ 9; 5 ]
   ; [ 10; 7 ]
   ; [ 11; 9 ]
   ; [ 12; 6; 4; 1 ]
   ; [ 13; 4; 3; 1 ]
   ; [ 14; 5; 3; 1 ]
   ; [ 15; 14 ]
   ; [ 16; 15; 13; 4 ]
   ; [ 17; 14 ]
   ; [ 18; 11 ]
   ; [ 19; 6; 2; 1 ]
   ; [ 20; 17 ]
   ; [ 21; 19 ]
   ; [ 22; 21 ]
   ; [ 23; 18 ]
   ; [ 24; 23; 22; 17 ]
   ; [ 25; 22 ]
   ; [ 26; 6; 2; 1 ]
   ; [ 27; 5; 2; 1 ]
   ; [ 28; 25 ]
   ; [ 29; 27 ]
   ; [ 30; 6; 4; 1 ]
   ; [ 31; 28 ]
   ; [ 32; 22; 2; 1 ]
   ; [ 33; 20 ]
   ; [ 34; 27; 2; 1 ]
   ; [ 35; 33 ]
   ; [ 36; 25 ]
   ; [ 37; 5; 4; 3; 2; 1 ]
   ; [ 38; 6; 5; 1 ]
   ; [ 39; 35 ]
   ; [ 40; 38; 21; 19 ]
   ; [ 41; 38 ]
   ; [ 42; 41; 20; 19 ]
   ; [ 43; 42; 38; 37 ]
   ; [ 44; 43; 18; 17 ]
   ; [ 45; 44; 42; 41 ]
   ; [ 46; 45; 26; 25 ]
   ; [ 47; 42 ]
   ; [ 48; 47; 21; 20 ]
   ; [ 49; 40 ]
   ; [ 50; 49; 24; 23 ]
   ; [ 51; 50; 36; 35 ]
   ; [ 52; 49 ]
   ; [ 53; 52; 38; 37 ]
   ; [ 54; 53; 18; 17 ]
   ; [ 55; 31 ]
   ; [ 56; 55; 35; 34 ]
   ; [ 57; 50 ]
   ; [ 58; 39 ]
   ; [ 59; 58; 38; 37 ]
   ; [ 60; 59 ]
   ; [ 61; 60; 46; 45 ]
   ; [ 62; 61; 6; 5 ]
   ; [ 63; 62 ]
   ; [ 64; 63; 61; 60 ]
   ; [ 65; 47 ]
   ; [ 66; 65; 57; 56 ]
   ; [ 67; 66; 58; 57 ]
   ; [ 68; 59 ]
   ; [ 69; 67; 42; 40 ]
   ; [ 70; 69; 55; 54 ]
   ; [ 71; 65 ]
   ; [ 72; 66; 25; 19 ]
   ; [ 73; 48 ]
   ; [ 74; 73; 59; 58 ]
   ; [ 75; 74; 65; 64 ]
   ; [ 76; 75; 41; 40 ]
   ; [ 77; 76; 47; 46 ]
   ; [ 78; 77; 59; 58 ]
   ; [ 79; 70 ]
   ; [ 80; 79; 43; 42 ]
   ; [ 81; 77 ]
   ; [ 82; 79; 47; 44 ]
   ; [ 83; 82; 38; 37 ]
   ; [ 84; 71 ]
   ; [ 85; 84; 58; 57 ]
   ; [ 86; 85; 74; 73 ]
   ; [ 87; 74 ]
   ; [ 88; 87; 17; 16 ]
   ; [ 89; 51 ]
   ; [ 90; 89; 72; 71 ]
   ; [ 91; 90; 8; 7 ]
   ; [ 92; 91; 80; 79 ]
   ; [ 93; 91 ]
   ; [ 94; 73 ]
   ; [ 95; 84 ]
   ; [ 96; 94; 49; 47 ]
   ; [ 97; 91 ]
   ; [ 98; 87 ]
   ; [ 99; 97; 54; 52 ]
   ; [ 100; 63 ]
   ; [ 101; 100; 95; 94 ]
   ; [ 102; 101; 36; 35 ]
   ; [ 103; 94 ]
   ; [ 104; 103; 94; 93 ]
   ; [ 105; 89 ]
   ; [ 106; 91 ]
   ; [ 107; 105; 44; 42 ]
   ; [ 108; 77 ]
   ; [ 109; 108; 103; 102 ]
   ; [ 110; 109; 98; 97 ]
   ; [ 111; 101 ]
   ; [ 112; 110; 69; 67 ]
   ; [ 113; 104 ]
   ; [ 114; 113; 33; 32 ]
   ; [ 115; 114; 101; 100 ]
   ; [ 116; 115; 46; 45 ]
   ; [ 117; 115; 99; 97 ]
   ; [ 118; 85 ]
   ; [ 119; 111 ]
   ; [ 120; 113; 9; 2 ]
   ; [ 121; 103 ]
   ; [ 122; 121; 63; 62 ]
   ; [ 123; 121 ]
   ; [ 124; 87 ]
   ; [ 125; 124; 18; 17 ]
   ; [ 126; 125; 90; 89 ]
   ; [ 127; 126 ]
   ; [ 128; 126; 101; 99 ]
   ; [ 129; 124 ]
   ; [ 130; 127 ]
   ; [ 131; 130; 84; 83 ]
   ; [ 132; 103 ]
   ; [ 133; 132; 82; 81 ]
   ; [ 134; 77 ]
   ; [ 135; 124 ]
   ; [ 136; 135; 11; 10 ]
   ; [ 137; 116 ]
   ; [ 138; 137; 131; 130 ]
   ; [ 139; 136; 134; 131 ]
   ; [ 140; 111 ]
   ; [ 141; 140; 110; 109 ]
   ; [ 142; 121 ]
   ; [ 143; 142; 123; 122 ]
   ; [ 144; 143; 75; 74 ]
   ; [ 145; 93 ]
   ; [ 146; 145; 87; 86 ]
   ; [ 147; 146; 110; 109 ]
   ; [ 148; 121 ]
   ; [ 149; 148; 40; 39 ]
   ; [ 150; 97 ]
   ; [ 151; 148 ]
   ; [ 152; 151; 87; 86 ]
   ; [ 153; 152 ]
   ; [ 154; 152; 27; 25 ]
   ; [ 155; 154; 124; 123 ]
   ; [ 156; 155; 41; 40 ]
   ; [ 157; 156; 131; 130 ]
   ; [ 158; 157; 132; 131 ]
   ; [ 159; 128 ]
   ; [ 160; 159; 142; 141 ]
   ; [ 161; 143 ]
   ; [ 162; 161; 75; 74 ]
   ; [ 163; 162; 104; 103 ]
   ; [ 164; 163; 151; 150 ]
   ; [ 165; 164; 135; 134 ]
   ; [ 166; 165; 128; 127 ]
   ; [ 167; 161 ]
   ; [ 168; 166; 153; 151 ]
  |]
;;

let max_lfsr_width = Array.length taps - 1

let counterpart =
  let counterpart = function
    | [] -> []
    | n :: t -> n :: List.map t ~f:(fun t -> n - t)
  in
  (* Taps must be sorted *)
  Array.map taps ~f:(fun d ->
    counterpart d |> List.sort ~compare:(fun a b -> -Int.compare a b))
;;

type 'a comb = (module Hardcaml.Comb.S with type t = 'a)

let galois (type a) ((module B) : a comb) xor taps state =
  let open B in
  let bit0 = state.:(0) in
  let rec f taps ~hi =
    match taps with
    | [] -> [ With_zero_width.select (Some state) ~high:hi ~low:1 ]
    | tap :: taps ->
      With_zero_width.select (Some state) ~high:hi ~low:(tap + 1)
      :: Some (xor bit0 state.:(tap))
      :: f taps ~hi:(tap - 1)
  in
  bit0
  @: With_zero_width.(
       concat_msb (f (List.tl_exn taps) ~hi:(width state - 1)) |> to_non_zero_width)
;;

let fibonacci (type a) ((module B) : a comb) xor taps state =
  let open B in
  let tap_bits = List.map taps ~f:(fun i -> state.:(i - 1)) in
  lsbs state @: List.reduce_exn tap_bits ~f:xor
;;

module Config = struct
  type t =
    | Galois
    | Fibonacci
  [@@deriving enumerate, sexp_of]
end

module Op = struct
  type t =
    | Xor
    | Xnor
  [@@deriving enumerate, sexp_of]
end

let create
  (type a)
  ?(config = Config.Galois)
  ?(counterpart_taps = false)
  ?(op = Op.Xor)
  ((module B) : a comb)
  d
  =
  let open B in
  let xor a b =
    match op with
    | Xor -> a ^: b
    | Xnor -> ~:(a ^: b)
  in
  let lfsr =
    match config with
    | Galois -> galois
    | Fibonacci -> fibonacci
  in
  let length = width d in
  if length < 2 || length > 168
  then raise_s [%message "LFSR length must be >=2 and <= 168" (length : int)];
  let taps = if counterpart_taps then counterpart.(length) else taps.(length) in
  lfsr (module B) xor taps d
;;
