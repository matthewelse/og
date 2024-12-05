open! Core
open! Import

module Id : sig
  type t : immediate64 [@@deriving compare, equal, sexp_of]

  include Hashable.S_plain with type t := t
  include Intable.S with type t := t
end

val root_state : Id.t

type t [@@deriving sexp_of]

val of_strings : string list -> t
val of_slices : Slice.t list @ local -> t
val is_accepting_state : t @ local -> Id.t -> bool

(** Returns [Id.root_state] if [char] does not have a child at this point. *)
val next_state : t @ local -> Id.t -> char -> Id.t

val iter_states : t @ local -> f:(Id.t -> unit) @ local -> unit
val iter_children : t @ local -> Id.t -> f:(char -> Id.t -> unit) @ local -> unit
val num_states : t @ local -> int
