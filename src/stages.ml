open Base
open Hardcaml

type 'a t =
  { input : 'a
  ; output : 'a
  }
[@@deriving sexp_of]

type 'a stage_fn = int -> 'a -> 'a

let create num_stages ~init ~f =
  if num_stages < 1
  then raise_s [%message "[Stages.create] requires at least 1 stage" (num_stages : int)];
  let rec loop n d =
    if n = num_stages
    then []
    else (
      let q = f n d in
      { input = d; output = q } :: loop (n + 1) q)
  in
  loop 0 init |> Array.of_list
;;

let input t = t.(0).input
let output t = t.(Array.length t - 1).output

let pipeline reg_spec num_stages ~enable ~init ~f =
  create num_stages ~init ~f:(fun n d -> Signal.reg reg_spec ~enable (f n d))
;;

type enabled_stage =
  { enable : Signal.t
  ; data : Signal.t
  }

let pipeline_with_enable reg_spec num_stages ~enable ~init ~f =
  create num_stages ~init:{ enable; data = init } ~f:(fun n { enable; data } ->
    { enable = Signal.reg reg_spec ~enable:Signal.vdd enable
    ; data = Signal.reg reg_spec ~enable (f n data)
    })
;;
