(** Core-ish shims for iarray *)

open! Core

type 'a t = 'a iarray [@@deriving sexp_of]

include module type of Stdlib_stable.IarrayLabels with type 'a t := 'a t

val fold : 'a iarray -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b
val unsafe_of_array : 'a array -> 'a iarray
