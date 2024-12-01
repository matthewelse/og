open! Core

module Flag : sig
  type t =
    | Require_sol
    | Require_eol
  [@@deriving compare, sexp_of]
end

type t [@@deriving sexp_of]

val singleton : Flag.t -> t
val mem : t -> Flag.t -> bool
val add : t -> Flag.t -> t
val empty : t
val of_list : Flag.t list -> t
