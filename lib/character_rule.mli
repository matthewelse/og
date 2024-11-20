open! Base
open! Import

type t =
  | Class of Character_class.t
  | One_of of char iarray
  | Not_one_of of char iarray
  | Literal of global_ Slice.Search_pattern.t
[@@deriving sexp_of]

(** [matches t ~input ~offset] returns the number of characters matched, if
    applicable, and returns [None] otherwise. *)
val matches : t @ local -> input:Slice.t @ local -> offset:int64# -> I64.Option.t @ local
