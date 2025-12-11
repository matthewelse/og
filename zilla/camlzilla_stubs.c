#include <assert.h>
#include <string.h>
#include <caml/bigarray.h>

#define BUILTIN(name)                                                                    \
  void name() { assert(!"Didn't use [@@builtin] intrinsic."); }

uintnat caml_native_pointer_of_bigstring(value v_bstr, intnat pos)
{
  return (uintnat)(((char *)Caml_ba_data_val(v_bstr)) + pos);
}

BUILTIN(caml_native_pointer_load_unsigned_int8)
