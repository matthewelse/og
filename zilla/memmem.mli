@@ portable

open! Base
open! Unboxed

module String : sig
  val memmem
    :  needle:string
    -> needle_pos:i64
    -> needle_len:i64
    -> haystack:string
    -> haystack_pos:i64
    -> haystack_len:i64
    -> I64.Option.t
end

module Bigstring : sig
  val memmem
    :  needle:Base_bigstring.t
    -> needle_pos:i64
    -> needle_len:i64
    -> haystack:Base_bigstring.t
    -> haystack_pos:i64
    -> haystack_len:i64
    -> I64.Option.t
end

module Bytes : sig
  val memmem
    :  needle:bytes
    -> needle_pos:i64
    -> needle_len:i64
    -> haystack:bytes
    -> haystack_pos:i64
    -> haystack_len:i64
    -> I64.Option.t
end
