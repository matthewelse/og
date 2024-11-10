open! Base
open! Import

type t =
  | Char of char
  | Class of Character_class.t
  | End_of_line
  | Group of char list
  | Neg_group of char list
  | Opt of t
  | Or of t * t
  | Rep1 of t
  | Seq of t list
  | Start_of_line
[@@deriving sexp_of]

val of_string : string -> t Or_error.t

module Compiled : sig
  type t

  val matches : t -> string -> bool
end

val compile : t -> Compiled.t
