#include "caml/alloc.h"
#include "stdio.h"
#include "string.h"
#include <dirent.h>
#include <assert.h>
#include <errno.h>

value og_utils_dirent_name_local(struct dirent* dirent) {
  mlsize_t len;
  value res;

  len = strlen(dirent->d_name);
  res = caml_alloc_local_string (len);
  memcpy((char *)String_val(res), dirent->d_name, len);
  return res;
}

value og_utils_dirent_is_regular_file(struct dirent* dirent) {
  return Val_bool(dirent->d_type == DT_REG);
}

value og_utils_explain_error() {
  printf("error: %s\n", strerror(errno));

  return Val_unit;
}
