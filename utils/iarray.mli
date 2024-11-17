(** Core-ish shims for iarray *)

open! Core

type ('a : any) t = 'a iarray [@@deriving sexp_of]

include module type of Stdlib_stable.IarrayLabels with type 'a t := 'a t

val fold : 'a iarray -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b

external unsafe_get : ('a : any). 'a iarray -> int -> 'a = "%array_unsafe_get"
[@@layout_poly] [@@noalloc]

external unsafe_of_array
  : ('a : any).
  ('a array[@local_opt]) -> ('a t[@local_opt])
  = "%identity"
[@@layout_poly] [@@noalloc]

[%%template:
  val construct
    : ('a : k).
    len:int -> default:'a -> f:('a array @ local -> unit) @ local -> 'a t
  [@@kind k = (value, bits64)]]

val construct_local
  :  len:int
  -> default:'a
  -> f:('a array @ local -> unit) @ local
  -> 'a t @ local
