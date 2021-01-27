#include <rlang.h>

static
r_ssize sizes_node = -1;
static
r_ssize sizes_vector = -1;

#include "decl/size-decl.h"


r_ssize sexp_self_size(sexp* x, enum r_type type) {
  if (ALTREP(x)) {
    r_abort("TODO: altrep size");
  }

  switch (type) {
  case r_type_null:
  case r_type_special:
  case r_type_builtin:
    // Technically these should return a node size. However we don't
    // count them because they are singletons only allocated when R is
    // initialised.
    return 0;

  case r_type_bytecode:
  case r_type_call:
  case r_type_closure:
  case r_type_dots:
  case r_type_environment:
  case r_type_pairlist:
  case r_type_pointer:
  case r_type_promise:
  case r_type_symbol:
  case r_type_s4:
  case r_type_weakref:
    return sizes_node;
  default:
    break;
  }

  r_ssize n = r_length(x);

  switch (type) {
  case r_type_logical:
  case r_type_integer: return sizes_vector + vec_size(n, sizeof(int));
  case r_type_double:  return sizes_vector + vec_size(n, sizeof(double));
  case r_type_complex: return sizes_vector + vec_size(n, sizeof(r_complex_t));
  case r_type_raw:     return sizes_vector + vec_size(n, 1);
  case r_type_string:  return sizes_vector + vec_size(n + 1, 1);
  case r_type_character:
  case r_type_expression:
  case r_type_list:    return sizes_vector + vec_size(n, sizeof(sexp*));
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


void init_size(sexp* node_size, sexp* vec_size) {
  sizes_node = r_as_int(node_size);
  sizes_vector = r_as_int(vec_size);
}
