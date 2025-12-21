#include "caml/alloc.h"
#include "stdio.h"
#include "stdint.h"
#include "string.h"
#include "dirent.h"
#include "errno.h"
#include "assert.h"

void mio_unreachable() { assert(!"bytecode is not supported"); }

int32_t mio_errno() {
  return errno;
}

value mio_explain_error() {
  printf("error: %s\n", strerror(errno));

  return Val_unit;
}


value mio_dirent_name_local(struct dirent* dirent) {
  mlsize_t len;
  value res;

  len = strlen(dirent->d_name);
  res = caml_alloc_local_string (len);
  memcpy((char *)String_val(res), dirent->d_name, len);
  return res;
}

value mio_dirent_is_regular_file(struct dirent* dirent) {
  return Val_bool(dirent->d_type == DT_REG);
}

