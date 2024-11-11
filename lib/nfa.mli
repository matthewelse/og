(** A representation of a nondeterministic finite automata (NFA) for regex
    matching (using the Thompson construction). *)

open! Core
open! Import

module State : sig
  (** An identifier representing a state in an NFA. *)

  type t : immediate64 [@@deriving sexp_of]
end

module Builder : sig
  type t [@@deriving sexp_of]

  (** Returns a new state id. Unique to this particular NFA. Not globally unique. *)
  val fresh_state : t @ local -> State.t

  (** [add_edge t ~from_state ~to_state rule] adds the specified rule to the NFA.
      If [rule] is [None], this will be an epsilon transition. *)
  val add_edge
    :  t @ local
    -> from_state:State.t
    -> to_state:State.t
    -> Character_rule.t option
    -> unit
end

type t [@@deriving sexp_of]

val build : (Builder.t @ local -> State.t) @ local -> t

(** Returns [true] if the provided string reaches the accepting state in the
    NFA. *)
val eval : t -> string -> bool
