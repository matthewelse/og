#include "caml/mlvalues.h"
#include "stdbool.h"
#include "string.h"

CAMLprim value slice_memcmp(value first, value second) {
    int64_t first_len     = (int64_t)Field(first, 2);
    int64_t second_len    = (int64_t)Field(second, 2);

    if (first_len != second_len) {
        return Val_bool(false);
    }

    int64_t len = first_len;

    const char* first_string = String_val(Field(first, 0));
    const char* second_string = String_val(Field(second, 0));

    int64_t first_pos    = (int64_t)(Field(first, 1));
    int64_t second_pos   = (int64_t)(Field(second, 1));

    return Val_bool(0 == memcmp(first_string + first_pos, second_string + second_pos, len));
}


CAMLprim value slice_memchr(value haystack, value needle) {
    const char* string = String_val(Field(haystack, 0));
    int64_t     pos    = (int64_t) (Field(haystack, 1));
    int64_t     len    = (int64_t) (Field(haystack, 2));

    char chr = Int_val(needle);
    
    const char* result = memchr(string + pos, chr, len);

    if (result == NULL) {
        return Val_int(-1);
    }

    return Val_int(result - (string + pos));
}
