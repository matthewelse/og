open! Core
include Array

external create_local : len:int -> 'a -> 'a array @ local = "caml_make_local_vect"

external unsafe_get : ('a : any). 'a array -> int -> 'a = "%array_unsafe_get"
[@@layout_poly] [@@noalloc]

external unsafe_set : ('a : any). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
[@@layout_poly] [@@noalloc]
