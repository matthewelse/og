open! Core
open! Import
include module type of Regex0

val of_string : string -> t Or_error.t

module Compiled : sig
  type t [@@deriving sexp_of]

  (* TODO: support matching natively on strings. *)

  val matches : t -> Slice.t @ local -> bool
end

(** [compile regex] compiles the provided regex into a non-deterministic finite
    automaton (NFA). *)
val compile : ?impl:Implementation.t -> t -> Compiled.t
