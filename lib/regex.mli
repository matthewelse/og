open! Core
open! Import

type t [@@deriving sexp_of]

val of_string : string -> t Or_error.t

module Compiled : sig
  type t

  val matches : t -> string -> bool
end

(** [compile regex] compiles the provided regex into a non-deterministic finite
    automaton (NFA). *)
val compile : t -> Compiled.t
