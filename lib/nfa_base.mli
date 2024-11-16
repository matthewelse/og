open! Core
open! Import

module State : sig
  (** An identifier representing a state in an NFA. *)

  type t : immediate64 [@@deriving compare, equal, sexp_of]
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
val accepting_state : t -> State.t

module Node : sig
  type t = (Character_rule.t option * State.t) iarray [@@deriving sexp_of]

  val exists : t -> f:(Character_rule.t option -> State.t -> bool) @ local -> bool
end

val node : t -> State.t -> Node.t
val initial_state : t -> State.t
val compile : Regex0.t -> t
