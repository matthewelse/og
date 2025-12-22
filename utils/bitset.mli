open! Core
open Unboxed

type t [@@deriving compare, hash, sexp_of]

val empty : t
val singleton : i64 -> t
val add : t -> i64 -> t
val mem : t -> i64 -> bool
val union : t -> t -> t
val iter : t -> f:(i64 -> unit) @ local -> unit
val fold : t -> init:'acc -> f:('acc -> i64 -> 'acc) @ local -> 'acc
val exists : t -> f:(i64 -> bool) @ local -> bool
val of_list : int list -> t
