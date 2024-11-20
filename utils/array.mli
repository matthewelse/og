open! Core
include module type of Array

val create_local : len:int -> 'a -> 'a array @ local

external unsafe_get : ('a : any). 'a array @ local -> int -> 'a = "%array_unsafe_get"
[@@layout_poly] [@@noalloc]

external unsafe_set
  : ('a : any).
  'a array @ local -> int -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly] [@@noalloc]

external set : ('a : any). 'a array @ local -> int -> 'a -> unit = "%array_safe_set"
[@@layout_poly] [@@noalloc]

val create__bits64 : ('a : bits64). len:int -> 'a -> 'a array
