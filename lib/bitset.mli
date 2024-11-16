open! Core

type t = int list [@@deriving compare, hash, sexp_of]

val empty : t
val singleton : int -> t
val add : t -> int -> t
val mem : t -> int -> bool
val union : t -> t -> t
val iter : t -> f:(int -> unit) @ local -> unit
val fold : t -> init:'acc -> f:('acc -> int -> 'acc) @ local -> 'acc
val exists : t -> f:(int -> bool) @ local -> bool
val of_list : int list -> t
