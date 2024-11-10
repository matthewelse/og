open! Core
open! Import

type t =
  { input : string
  ; mutable offset : int
  }

val create : string -> t
val peek : t -> char option @ local
val take : t -> char option @ local
val is_empty : t -> bool
val drop_exn : t -> unit
