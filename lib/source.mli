open! Core
open! Import

type t =
  | Stdin
  | Files_recursively_under of string
[@@deriving sexp_of]

val should_ignore : string -> bool

val iter
  :  t
  -> buffer_size:int
  -> max_buffer_size:int
  -> f:(string -> Import.Buffered_reader.t -> unit)
  -> unit
