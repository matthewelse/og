open! Base
open! Import

type t =
  | Class of Character_class.t
  | One_of of char array
  | Not_one_of of char array
  | Start_of_line
  | End_of_line
[@@deriving sexp_of]

(** [matches t ~input ~offset] returns the number of characters matched, if
    applicable, and returns [None] otherwise. *)
val matches : t -> input:string -> offset:int -> int option
