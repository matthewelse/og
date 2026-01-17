open! Core
open! Unboxed
open! Import

type t =
  | Class of Character_class.t
  | One_of of char iarray
  | Not_one_of of char iarray
  | Literal of global_ Slice.Search_pattern.t
[@@deriving sexp_of]

let matches (t @ local) ~input ~offset : I64.Option.t =
  match t with
  | Class character_class ->
    (match Slice.get input offset with
     | Null -> I64.Option.none ()
     | This c ->
       if Character_class.matches character_class c
       then I64.Option.some #1L
       else I64.Option.none ())
  | One_of chars ->
    (match Slice.get input offset with
     | Null -> I64.Option.none ()
     | This c ->
       if Iarray.Local.exists chars ~f:(fun c' -> Char.equal c c')
       then I64.Option.some #1L
       else I64.Option.none ())
  | Not_one_of chars ->
    (match Slice.get input offset with
     | Null -> I64.Option.none ()
     | This c ->
       if not (Iarray.Local.exists chars ~f:(fun c' -> Char.equal c c'))
       then I64.Option.some #1L
       else I64.Option.none ())
  | Literal l ->
    let substring = Slice.Search_pattern.pattern l in
    (match%optional_u.Slice.Option
       Slice.slice input ~pos:offset ~len:(Slice.length substring)
     with
     | None -> I64.Option.none ()
     | Some input_substring ->
       if Slice.memcmp substring input_substring
       then I64.Option.some (Slice.length substring)
       else I64.Option.none ())
;;
