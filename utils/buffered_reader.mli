open! Core
open Unboxed

exception Buffer_limit_exceeded

type t

val stdin : ?initial_size:int -> max_size:int -> unit -> t
val of_fd : ?initial_size:int -> max_size:int -> Core_unix.File_descr.t -> t
val of_string : ?initial_size:int -> max_size:int -> string -> t

(** [line t ~f] returns a single line from the underlying file descriptor. Each slice will
    _not_ include a trailing newline character. *)
val line : t -> f:(Slice.t @ local -> 'a) @ local -> 'a

val ensure_line : t -> unit

(** [chunk t ~f] returns a single chunk from the underlying file descriptor, many lines at
    a time. Returns a whole number of lines. *)
val chunk : t -> f:(Slice.t @ local -> 'a) @ local -> 'a

val ensure : t -> i64 -> unit
val consume : t -> i64 -> unit
val peek : t -> Slice.t @ local

(** Returns [true] if this file is "probably binary", based on some heuristic that looks
    at the first [num_bytes_to_read] bytes. *)
val is_probably_binary : t -> num_bytes_to_read:i64 -> bool
