open! Base
open! Unboxed

type t = int16x8#
type mask = int16x8#

external const
  :  int16#
  -> int16#
  -> int16#
  -> int16#
  -> int16#
  -> int16#
  -> int16#
  -> int16#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_int16x8_const8"
[@@noalloc] [@@builtin]

external set1
  :  int64#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int16x8_dup_n"
[@@noalloc] [@@builtin]

external ( = )
  :  t
  -> t
  -> mask
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int16x8_cmpeq"
[@@noalloc] [@@builtin]

external extract
  :  (int[@untagged])
  -> t
  -> int16#
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int16x8_extract"
[@@noalloc] [@@builtin]

external vshrn_n
  :  (int[@untagged])
  -> t
  -> int64x2#
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_neon_int8x16_shrn"
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

module String = Load_store.String_Int16x8
module Bytes = Load_store.Bytes_Int16x8
module Bigstring = Load_store.Bigstring_Int16x8
module Raw = Load_store.Raw_Int16x8
