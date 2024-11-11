(** Core-ish shims for iarray *)

open! Core

type 'a t = 'a iarray [@@deriving sexp_of]

val exists : 'a iarray -> f:('a -> bool) @ local -> bool
val of_list : 'a list -> 'a iarray
val fold : 'a iarray -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b

(** TODO: [of_array : 'a array @ unique -> 'a t] *)
val unsafe_of_array : 'a array -> 'a t

val get_exn : 'a t -> int -> 'a
val map : 'a t -> f:('a -> 'b) @ local -> 'b t
val find_map_local : 'a t -> f:('a -> 'b option @ local) @ local -> 'b option @ local
