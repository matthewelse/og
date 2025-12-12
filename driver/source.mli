open! Core
open! Import

type t =
  | Stdin
  | Files_recursively_under of string
[@@deriving sexp_of]

val iter
  :  t
  -> buffer_size:int
  -> max_buffer_size:int
  -> f:(string -> Buffered_reader.t -> unit)
  -> unit
