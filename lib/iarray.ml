open! Core
include Stdlib_stable.IarrayLabels

type ('a : any) t = 'a iarray

let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)
let fold t ~init ~f = fold_left t ~init ~f

external unsafe_of_array : ('a : any). 'a array -> 'a t = "%identity"
[@@layout_poly] [@@noalloc]
