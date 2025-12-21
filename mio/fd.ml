open! Base
open! Unboxed

type t = i32

let stdin () = #0l
let stdout () = #1l
let stderr () = #2l

external open_ : path:string -> flags:i32 -> t @@ portable = "mio_unreachable" "open"
external close : t -> i32 @@ portable = "mio_unreachable" "close"

let close t = close t |> (ignore : i32 -> unit)

external unsafe_read_assume_fd_is_nonblocking
  :  t
  -> iptr
  -> len:i64
  -> i64
  @@ portable
  = "mio_unreachable" "read"
[@@noalloc]
