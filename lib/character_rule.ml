open! Core
open! Import

type t =
  | Class of Character_class.t
  | One_of of char iarray
  | Not_one_of of char iarray
  | Start_of_line
  | End_of_line
  | Literal of global_ Slice.Search_pattern.t
[@@deriving sexp_of]

let matches t ~input ~offset = exclave_
  match t with
  | Class character_class ->
    if offset < Slice.length input
       && Character_class.matches character_class (Slice.at input offset)
    then Some 1
    else None
  | One_of chars ->
    if offset < Slice.length input
       && Iarray.exists_local chars ~f:(fun c -> Char.equal c (Slice.at input offset))
    then Some 1
    else None
  | Not_one_of chars ->
    if offset < Slice.length input
       && not
            (Iarray.exists_local chars ~f:(fun c -> Char.equal c (Slice.at input offset)))
    then Some 1
    else None
  | Start_of_line -> if offset = 0 then Some 0 else None
  | End_of_line -> if offset = Slice.length input then Some 0 else None
  | Literal l ->
    let substring = Slice.Search_pattern.pattern l in
    if Slice.length input >= Slice.length substring
       && Slice.memcmp
            (Slice.slice ~pos:offset ~len:(Slice.length substring) input)
            substring
    then Some (Slice.length substring)
    else None
;;
