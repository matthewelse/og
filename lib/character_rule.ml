open! Core
open! Import

type t =
  | Class of Character_class.t
  | One_of of char iarray
  | Not_one_of of char iarray
  | Start_of_line
  | End_of_line
  | Literal of global_ String.Search_pattern.t
[@@deriving sexp_of]

let matches t ~input ~offset = exclave_
  match t with
  | Class character_class ->
    if offset < String.length input
       && Character_class.matches character_class input.[offset]
    then Some 1
    else None
  | One_of chars ->
    if offset < String.length input
       && Iarray.exists_local chars ~f:(fun c -> Char.equal c input.[offset])
    then Some 1
    else None
  | Not_one_of chars ->
    if offset < String.length input
       && not (Iarray.exists_local chars ~f:(fun c -> Char.equal c input.[offset]))
    then Some 1
    else None
  | Start_of_line -> if offset = 0 then Some 0 else None
  | End_of_line -> if offset = String.length input then Some 0 else None
  | Literal l ->
    let substring = String.Search_pattern.pattern l in
    if String.is_substring_at input ~pos:offset ~substring
    then Some (String.length substring)
    else None
;;
