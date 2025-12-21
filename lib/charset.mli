open! Core

type t : bits64 & bits64 & bits64 & bits64 [@@deriving sexp_of]

val of_list : char list -> t
val mem : t -> char -> bool
