open! Core
include Stdlib_stable.IarrayLabels

type ('a : any) t = 'a iarray

let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)
let fold t ~init ~f = fold_left t ~init ~f

external unsafe_of_array
  : ('a : any).
  ('a array[@local_opt]) -> ('a t[@local_opt])
  = "%identity"
[@@layout_poly] [@@noalloc]

external unsafe_get : ('a : any). 'a iarray -> int -> 'a = "%array_unsafe_get"
[@@layout_poly] [@@noalloc]

let%template construct (type a : k) ~len ~(default : a) ~f =
  let a : a array = (Og_utils__Array.create [@kind k]) ~len default in
  f a;
  unsafe_of_array a
[@@kind k = (value, bits64)]
;;

let construct_local ~len ~default ~f = exclave_
  let a = Array.create_local ~len default in
  f a;
  unsafe_of_array a
;;
