open Base
open Hardcaml

module Architecture = struct
  type t =
    | Combinational
    | Pipelined
    | Iterative
  [@@deriving sexp_of, enumerate]
end

module Config = struct
  type t =
    { architecture : Architecture.t
    ; iterations : int
    }
  [@@deriving sexp_of]
end

module type S = sig
  module I : sig
    type 'a t =
      { clk : 'a
      ; clr : 'a
      ; enable : 'a
      ; ld : 'a
      ; system : 'a
      ; mode : 'a
      ; c : 'a
      ; x : 'a
      ; y : 'a
      ; z : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { xo : 'a
      ; yo : 'a
      ; zo : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Config.t -> Signal.t I.t -> Signal.t O.t
end

module type Cordic = sig
  module type S = S

  module Architecture = Architecture
  module Config = Config

  module System : sig
    include module type of struct
      include Cordic_reference.System
    end

    val to_signal : t -> Signal.t
  end

  module Mode : sig
    include module type of struct
      include Cordic_reference.Mode
    end

    val to_signal : t -> Signal.t
  end

  module Make (Fixnum_spec : Fixnum.Spec) : S
end
