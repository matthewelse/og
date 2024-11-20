open! Core
include Array

external create_local : len:int -> 'a -> 'a array @ local = "caml_make_local_vect"

external unsafe_get : ('a : any). 'a array @ local -> int -> 'a = "%array_unsafe_get"
[@@layout_poly] [@@noalloc]

external unsafe_set
  : ('a : any).
  'a array @ local -> int -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly] [@@noalloc]

external set : ('a : any). 'a array @ local -> int -> 'a -> unit = "%array_safe_set"
[@@layout_poly] [@@noalloc]

external create_u64_uninitialized
  : ('a : bits64).
  len:int -> 'a array
  = "caml_make_unboxed_int64_vect_bytecode" "caml_make_unboxed_int64_vect"

let create__bits64 ~len default =
  let arr = create_u64_uninitialized ~len in
  for i = 0 to len - 1 do
    unsafe_set arr i default
  done;
  arr
;;
