open! Base
open! Unboxed

module String : sig
  val memchr : string -> pos:i64 -> len:i64 -> char -> I64.Option.t
  val memchr2 : string -> pos:i64 -> len:i64 -> char -> char -> I64.Option.t
  val memchr3 : string -> pos:i64 -> len:i64 -> char -> char -> char -> I64.Option.t
end

module Bigstring : sig
  val memchr : Base_bigstring.t -> pos:i64 -> len:i64 -> char -> I64.Option.t
  val memchr2 : Base_bigstring.t -> pos:i64 -> len:i64 -> char -> char -> I64.Option.t

  val memchr3
    :  Base_bigstring.t
    -> pos:i64
    -> len:i64
    -> char
    -> char
    -> char
    -> I64.Option.t
end

module Bytes : sig
  val memchr : Bytes.t -> pos:i64 -> len:i64 -> char -> I64.Option.t
  val memchr2 : Bytes.t -> pos:i64 -> len:i64 -> char -> char -> I64.Option.t
  val memchr3 : Bytes.t -> pos:i64 -> len:i64 -> char -> char -> char -> I64.Option.t
end
