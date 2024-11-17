open! Core

type 'a t [@@deriving sexp_of]

val create : 'a -> 'a t
val ( .:() ) : 'a t -> char -> 'a
val ( .:()<- ) : 'a t -> char -> 'a -> unit
