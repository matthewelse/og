open! Base
open! Import

type t =
  | Class of Character_class.t
  | One_of of char array
  | Not_one_of of char array
  | Start_of_line
  | End_of_line
[@@deriving sexp_of]

let matches t ~input ~offset =
  match t with
  | Class character_class ->
    if offset < String.length input
       && Character_class.matches character_class input.[offset]
    then Some 1
    else None
  | One_of chars ->
    if offset < String.length input
       && Array.exists chars ~f:(fun c -> Char.equal c input.[offset])
    then Some 1
    else None
  | Not_one_of chars ->
    if offset < String.length input
       && not (Array.exists chars ~f:(fun c -> Char.equal c input.[offset]))
    then Some 1
    else None
  | Start_of_line -> if offset = 0 then Some 0 else None
  | End_of_line -> if offset = String.length input then Some 0 else None
;;
