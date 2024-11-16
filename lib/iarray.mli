(** Core-ish shims for iarray *)

open! Core

type ('a : any) t = 'a iarray [@@deriving sexp_of]

include module type of Stdlib_stable.IarrayLabels with type 'a t := 'a t

val fold : 'a iarray -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b

external unsafe_of_array : ('a : any). 'a array -> 'a t = "%identity"
[@@layout_poly] [@@noalloc]
