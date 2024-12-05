(** Core-ish shims for iarray *)

open! Core

type ('a : any) t = 'a iarray [@@deriving sexp_of]

val sexp_of_t__bits64 : (('a : bits64) -> Sexp.t) -> 'a t -> Sexp.t

include module type of Stdlib_stable.IarrayLabels with type 'a t := 'a t

val fold : 'a iarray -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b

external length : ('a : any). 'a iarray @ local -> int = "%array_length"
[@@layout_poly] [@@noalloc]

external unsafe_get
  : ('a : any).
  ('a iarray[@local_opt]) -> int -> ('a[@local_opt])
  = "%array_unsafe_get"
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
