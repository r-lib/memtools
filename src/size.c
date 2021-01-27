#include <rlang.h>

double sexp_size(sexp* x, enum r_type type) {
  switch (type) {
  case r_type_null:
  case r_type_special:
  case r_type_builtin:
    return 0;
  default:
    r_abort("TODO");
  }
}

// [[ register() ]]
sexp* r_sexp_size(sexp* x) {
  return r_dbl(sexp_size(x, r_typeof(x)));
}
