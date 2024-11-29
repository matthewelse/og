open! Core

external count_leading_zeros
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_clz" "caml_int64_clz_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external count_leading_zeros_nonzero_arg
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_clz" "caml_int64_clz_nonzero_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external count_trailing_zeros
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_ctz" "caml_int64_ctz_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external count_trailing_zeros_nonzero_arg
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_ctz" "caml_int64_ctz_nonzero_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
