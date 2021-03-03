#include <rlang.h>

static
r_ssize sizes_node = -1;
static
r_ssize sizes_vector = -1;

#include "decl/size-decl.h"


r_ssize sexp_self_size(sexp* x, enum r_type type) {
  if (ALTREP(x)) {
    return 0;
    r_abort("TODO: altrep size");
  }

  switch (type) {
  case R_TYPE_null:
  case R_TYPE_special:
  case R_TYPE_builtin:
    // Technically these should return a node size. However we don't
    // count them because they are singletons only allocated when R is
    // initialised.
    return 0;

  case R_TYPE_bytecode:
  case R_TYPE_call:
  case R_TYPE_closure:
  case R_TYPE_dots:
  case R_TYPE_environment:
  case R_TYPE_pairlist:
  case R_TYPE_pointer:
  case R_TYPE_promise:
  case R_TYPE_symbol:
  case R_TYPE_s4:
  case R_TYPE_weakref:
    return sizes_node;
  default:
    break;
  }

  r_ssize n = r_length(x);

  switch (type) {
  case R_TYPE_logical:
  case R_TYPE_integer: return sizes_vector + vec_size(n, sizeof(int));
  case R_TYPE_double:  return sizes_vector + vec_size(n, sizeof(double));
  case R_TYPE_complex: return sizes_vector + vec_size(n, sizeof(r_complex_t));
  case R_TYPE_raw:     return sizes_vector + vec_size(n, 1);
  case R_TYPE_string:  return sizes_vector + vec_size(n + 1, 1);
  case R_TYPE_character:
  case R_TYPE_expression:
  case R_TYPE_list:    return sizes_vector + vec_size(n, sizeof(sexp*));
  default:             r_abort("Unsupported type in `sexp_self_size()`.");
  }
}

static
r_ssize vec_size(r_ssize n, size_t elt_byte_size) {
  if (n == 0) {
    return 0;
  }

  float heap_atom_byte_size = r_ssize_max(sizeof(sexp*), sizeof(double));
  float elements_per_atom = heap_atom_byte_size / elt_byte_size;
  r_ssize n_atoms = ceil(n / elements_per_atom);

  // Big vectors always allocated in 8 byte chunks
  if      (n_atoms > 16) return n_atoms * 8;
  // For small vectors, round to sizes allocated in small vector pool
  else if (n_atoms > 8)  return 128;
  else if (n_atoms > 6)  return 64;
  else if (n_atoms > 4)  return 48;
  else if (n_atoms > 2)  return 32;
  else if (n_atoms > 1)  return 16;
  else if (n_atoms > 0)  return 8;

  return 0;
}


// [[ register() ]]
sexp* r_sexp_self_size(sexp* x) {
  return r_dbl(sexp_self_size(x, r_typeof(x)));
}


void init_library_size(sexp* node_size, sexp* vec_size) {
  sizes_node = r_as_int(node_size);
  sizes_vector = r_as_int(vec_size);
}
