open! Core
open! Import

type t =
  | Class of Character_class.t
  | One_of of char iarray
  | Not_one_of of char iarray
  | Literal of global_ Slice.Search_pattern.t
[@@deriving sexp_of]

let matches t ~input ~offset =
  match t with
  | Class character_class ->
    (match Slice.at input offset with
     | None -> None
     | Some c -> if Character_class.matches character_class c then Some 1 else None)
  | One_of chars ->
    (match Slice.at input offset with
     | None -> None
     | Some c ->
       if Iarray.exists_local chars ~f:(fun c' -> Char.equal c c') then Some 1 else None)
  | Not_one_of chars ->
    (match Slice.at input offset with
     | None -> None
     | Some c ->
       if not (Iarray.exists_local chars ~f:(fun c' -> Char.equal c c'))
       then Some 1
       else None)
  | Literal l ->
    let substring = Slice.Search_pattern.pattern l in
    (match Slice.slice input ~pos:offset ~len:(Slice.length substring) with
     | None -> None
     | Some input_substring ->
       if Slice.memcmp substring input_substring
       then (
         let length = Slice.length substring in
         exclave_ Some length)
       else None)
;;
