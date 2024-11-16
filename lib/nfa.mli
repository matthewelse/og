(** A representation of a nondeterministic finite automata (NFA) for regex
    matching (using the Thompson construction). *)

open! Core
open! Import
module State = Nfa_base.State

type t [@@deriving sexp_of]

(** Returns [true] if the provided string reaches the accepting state in the
    NFA. *)
val eval : t -> Slice.t @ local -> bool

val compile : Regex0.t -> t
