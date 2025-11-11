open! Base
open! Unboxed

type t = int64x2#
type mask = int64x2#

external const
  :  int64#
  -> int64#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_int64x2_const2"
[@@noalloc] [@@builtin]

external set1
  :  int64#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int64x2_dup_n"
[@@noalloc] [@@builtin]

external ( = )
  :  t
  -> t
  -> mask
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int64x2_cmpeq"
[@@noalloc] [@@builtin]

external extract
  :  (int[@untagged])
  -> t
  -> i64
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int64x2_extract"
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

module String = Load_store.String_Int64x2
module Bytes = Load_store.Bytes_Int64x2
module Bigstring = Load_store.Bigstring_Int64x2
module Raw = Load_store.Raw_Int64x2
