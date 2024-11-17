open! Core

type 'a t = 'a array [@@deriving sexp_of]

val create : 'a -> 'a t
val ( .:() ) : 'a t -> char -> 'a
val ( .:()<- ) : 'a t -> char -> 'a -> unit
