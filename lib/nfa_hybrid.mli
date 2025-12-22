open! Core
open! Import

type t [@@deriving sexp_of]

val eval : t -> Slice.t @ local -> bool
val compile : Regex0.t -> t
val look_for_constant : t -> Bigstring.t or_null
