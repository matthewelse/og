open! Core
open! Import

module State : sig
  (** An identifier representing a state in a DFA. *)

  type t : immediate64 [@@deriving sexp_of]
end

module Builder : sig
  type t [@@deriving sexp_of]

  val fresh_state : t -> State.t
  val add_edge : t -> from_state:State.t -> to_state:State.t -> Character_rule.t -> unit
end

type t [@@deriving sexp_of]

val build : (Builder.t @ local -> State.t) @ local -> t

(** Returns [true] if the provided string reaches the accepting state in the
    DFA. *)
val eval : t -> Slice.t @ local -> offset:int -> bool
