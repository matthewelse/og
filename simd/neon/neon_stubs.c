#include <assert.h>

void ocaml_simd_neon_unreachable() { assert(!"SIMD is not supported in bytecode mode."); }

#define BUILTIN(name)                                                                    \
  void name() { assert(!"Didn't use [@@builtin] intrinsic."); }

BUILTIN(caml_neon_int8x16_cmpeq)
BUILTIN(caml_neon_int8x16_dup_n)
BUILTIN(caml_neon_int8x16_extract)
BUILTIN(caml_neon_int8x16_shrn)
BUILTIN(caml_neon_int8x16_maxv)
BUILTIN(caml_neon_int8x16_minv)
BUILTIN(caml_neon_int8x16_bitwise_or)

BUILTIN(caml_neon_int16x8_cmpeq)
BUILTIN(caml_neon_int16x8_dup_n)
BUILTIN(caml_neon_int16x8_extract)

BUILTIN(caml_neon_int32x4_cmpeq)
BUILTIN(caml_neon_int32x4_dup_n)
BUILTIN(caml_neon_int32x4_extract)

BUILTIN(caml_neon_int64x2_cmpeq)
BUILTIN(caml_neon_int64x2_dup_n)
BUILTIN(caml_neon_int64x2_extract)

BUILTIN(caml_int8x16_const16)
BUILTIN(caml_int8x16_const1)
BUILTIN(caml_int16x8_const8)
BUILTIN(caml_int32x4_const4)
BUILTIN(caml_int64x2_const2)

BUILTIN(caml_vec128_cast)

BUILTIN(caml_neon_load_aligned)
BUILTIN(caml_neon_load_unaligned)
BUILTIN(caml_neon_store_aligned)
BUILTIN(caml_neon_store_unaligned)

BUILTIN(caml_native_pointer_store_vec128_unaligned)
BUILTIN(caml_native_pointer_store_vec128_aligned)
BUILTIN(caml_native_pointer_load_vec128_unaligned)
BUILTIN(caml_native_pointer_load_vec128_aligned)
