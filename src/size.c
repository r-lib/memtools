#include <rlang.h>

static
r_ssize sizes_node = -1;
static
r_ssize sizes_vector = -1;

double sexp_self_size(sexp* x, enum r_type type) {
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
sexp* r_sexp_self_size(sexp* x) {
  return r_dbl(sexp_self_size(x, r_typeof(x)));
}


void init_size(sexp* node_size, sexp* vec_size) {
  sizes_node = r_as_int(node_size);
  sizes_vector = r_as_int(vec_size);
}
