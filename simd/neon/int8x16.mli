@@ portable

open! Base
open! Unboxed

type t = int8x16# [@@deriving sexp_of]
type mask = int8x16#

external const
  :  int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> int8#
  -> t
  = "ocaml_simd_neon_unreachable" "caml_int8x16_const16"
[@@noalloc] [@@builtin]

external const1
  :  int8#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_int8x16_const1"
[@@noalloc] [@@builtin]

external set1 : i64 -> t = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_dup_n"
[@@noalloc] [@@builtin]

external ( = ) : t -> t -> mask = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_cmpeq"
[@@noalloc] [@@builtin]

external ( land )
  :  t
  -> t
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_bitwise_and"
[@@noalloc] [@@builtin]

external ( lor )
  :  t
  -> t
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_bitwise_or"
[@@noalloc] [@@builtin]

external extract
  :  (int[@untagged])
  -> t
  -> int8#
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_extract"
[@@noalloc] [@@builtin]

external of_int8x16_bits
  :  int8x16#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external of_int16x8_bits
  :  int16x8#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external of_int32x4_bits
  :  int32x4#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

external of_int64x2_bits
  :  int64x2#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_vec128_cast"
[@@noalloc] [@@builtin]

val maxv : t -> i64
val minv : t -> i64

module String = Load_store.String_Int8x16
module Bytes = Load_store.Bytes_Int8x16
module Bigstring = Load_store.Bigstring_Int8x16
module Raw = Load_store.Raw_Int8x16
