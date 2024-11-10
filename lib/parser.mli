open! Base
open! Import

type t =
  { input : string
  ; mutable offset : int
  }

val create : string -> t
val peek : t -> char option
val take : t -> char option
val is_empty : t -> bool
