open! Core
open! Import

type t [@@deriving sexp_of]

val of_string : string -> t Or_error.t

module Compiled : sig
  type t [@@deriving sexp_of]

  val matches : t -> Slice.t @ local -> bool
end

(** [compile regex] compiles the provided regex into a non-deterministic finite
    automaton (NFA). *)
val compile : ?impl:Implementation.t -> t -> Compiled.t
