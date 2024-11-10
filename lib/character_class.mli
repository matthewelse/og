open! Base
open! Import

type t =
  | Alphanumeric
  | Numeric
[@@deriving sexp_of]

(** [matches t c] returns [true] if [c] is a member of the specified character
    class. *)
val matches : t -> char -> bool
