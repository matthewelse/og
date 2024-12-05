open! Core
include Stdlib_stable.IarrayLabels

type ('a : any) t = 'a iarray

external unsafe_get
  : ('a : any).
  ('a iarray[@local_opt]) -> int -> ('a[@local_opt])
  = "%array_unsafe_get"
[@@layout_poly] [@@noalloc]

external length : ('a : any). 'a iarray @ local -> int = "%array_length"
[@@layout_poly] [@@noalloc]

let fold t ~init ~f = fold_left t ~init ~f

let%template sexp_of_t (type a : k) (sexp_of_a : a -> Sexp.t) (t : a t) =
  let r = ref [] in
  for i = length t - 1 downto 0 do
    r := (unsafe_get t i |> sexp_of_a) :: !r
  done;
  !r |> [%sexp_of: Sexp.t list]
[@@kind k = (value, bits64)]
;;

external unsafe_of_array
  : ('a : any).
  ('a array[@local_opt]) -> ('a t[@local_opt])
  = "%identity"
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
