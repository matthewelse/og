@@ portable

(** A unix file descriptor. *)

open! Base
open! Unboxed

type t : bits32

(* FIXME: is it portable to expose stdin, etc. like this? *)

val stdin : unit -> t
val stdout : unit -> t
val stderr : unit -> t

(* FIXME: error handling *)
val open_ : path:string -> flags:i32 -> t
val close : t -> unit

(* Calls the [read] syscall under the hood. The return value has the same semantics as glibc's
   [read]. *)
val unsafe_read_assume_fd_is_nonblocking : t -> iptr -> len:i64 -> i64
