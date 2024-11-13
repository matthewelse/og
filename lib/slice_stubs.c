#include "caml/mlvalues.h"
#include "stdbool.h"
#include "string.h"

CAMLprim value slice_memcmp(value first, value second) {
    value first_len     = Field(first, 2);
    value second_len    = Field(second, 2);

    if (first_len != second_len) {
        return Val_bool(false);
    }

    int len = Int_val(first_len);

    const char* first_string = String_val(Field(first, 0));
    const char* second_string = String_val(Field(second, 0));

    int first_pos    = Int_val(Field(first, 1));
    int second_pos   = Int_val(Field(second, 1));

    return Val_bool(0 == memcmp(first_string + first_pos, second_string + second_pos, len));
}
