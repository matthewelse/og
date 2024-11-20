open! Core

exception Buffer_limit_exceeded

type t

val of_fd : ?initial_size:int -> max_size:int -> Core_unix.File_descr.t -> t
val line : t -> f:(Slice.t @ local -> 'a) @ local -> 'a
val stdin : ?initial_size:int -> max_size:int -> unit -> t
val ensure : t -> I64.t -> unit
val peek : t -> Slice.Bytes.t @ local
