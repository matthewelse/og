open! Base
open! Import

module State : sig
  type t [@@immediate64] [@@deriving sexp_of]
end

module Builder : sig
  type t [@@deriving sexp_of]

  (** Returns a new state id. Unique to this particular NFA. Not globally unique. *)
  val fresh_state : t -> State.t

  (** [add_edge t ~from_state ~to_state rule] adds the specified rule to the NFA.
      If [rule] is [None], this will be an epsilon transition. *)
  val add_edge
    :  t
    -> from_state:State.t
    -> to_state:State.t
    -> Character_rule.t option
    -> unit
end

type t [@@deriving sexp_of]

val build : (Builder.t -> State.t) -> t

(** Returns [true] if the provided string reaches the accepting state in the NFA. *)
val eval : t -> string -> offset:int -> bool
