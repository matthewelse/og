@@ portable

open! Core
open Unboxed

type t

val of_fd : Fd.t -> t
val of_string : string -> t

module Read_result : sig
  type t =
    | Ok of i64
    | Eof
    | Error of i32
  [@@deriving sexp_of ~stackify]
end

(** [read t slice] reads up to [Slice.length slice] bytes from the underlying input
    source.

    Returns the number of bytes read. Returns [I64.Option.none ()] if we've reached EOF. *)
val read : t -> Slice.t @ local -> Read_result.t @ local
