open! Base
open! Import

type t =
  | Class of Character_class.t
  | One_of of char iarray
  | Not_one_of of char iarray
  | Start_of_line
  | End_of_line
  | Literal of global_ String.Search_pattern.t
[@@deriving sexp_of]

(** [matches t ~input ~offset] returns the number of characters matched, if
    applicable, and returns [None] otherwise. *)
val matches : t @ local -> input:string -> offset:int -> int option @ local
