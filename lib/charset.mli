open! Core

type t [@@deriving sexp_of]

val of_list : char list -> t
val mem : t -> char -> bool
