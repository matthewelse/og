open! Base
open! Import

type t =
  | Alphanumeric
  | Numeric
[@@deriving sexp_of]

let matches t char =
  match t with
  | Alphanumeric -> Char.is_alphanum char
  | Numeric -> Char.is_digit char
;;
