@@ portable

open! Base
open! Unboxed

type t = int32x4#
type mask = int32x4#

external const
  :  int32#
  -> int32#
  -> int32#
  -> int32#
  -> t
  @@ portable
  = "ocaml_simd_neon_unreachable" "caml_int32x4_const4"
[@@noalloc] [@@builtin]

external set1 : int8# -> t = "ocaml_simd_neon_unreachable" "caml_neon_int32x4_dup_n"
[@@noalloc] [@@builtin]

external ( = ) : t -> t -> mask = "ocaml_simd_neon_unreachable" "caml_neon_int32x4_cmpeq"
[@@noalloc] [@@builtin]

external extract
  :  (int[@untagged])
  -> t
  -> int8#
  = "ocaml_simd_neon_unreachable" "caml_neon_int32x4_extract"
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

module String = Load_store.String_Int32x4
module Bytes = Load_store.Bytes_Int32x4
module Bigstring = Load_store.Bigstring_Int32x4
module Raw = Load_store.Raw_Int32x4
