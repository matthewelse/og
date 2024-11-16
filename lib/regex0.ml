open! Core
open! Import

type t =
  | String of string
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
