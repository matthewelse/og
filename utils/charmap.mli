open! Core

type ('a : value_or_null) t [@@deriving sexp_of]

val create : ('a : value_or_null). 'a -> 'a t
val ( .:() ) : ('a : value_or_null). 'a t -> char -> 'a
val ( .:()<- ) : ('a : value_or_null). 'a t -> char -> 'a -> unit
