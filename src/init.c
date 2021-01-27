#include <rlang.h>

// [[ register() ]]
sexp* init_memtools() {
  void init_snapshot();
  init_snapshot();
  return r_null;
}

sexp* r_sexp_self_size(sexp* addr);
sexp* addr_deref(sexp* addr);
sexp* snapshot(sexp* x);

static
const R_CallMethodDef r_callables[] = {
  {"c_ptr_init_library",           (r_void_fn) &r_init_library, 1},
  {"c_ptr_init_memtools",          (r_void_fn) &init_memtools, 1},
  {"c_ptr_addr_deref",             (r_void_fn) &addr_deref, 1},
  {"c_ptr_sexp_self_size",         (r_void_fn) &r_sexp_self_size, 1},
  {"c_ptr_snapshot",               (r_void_fn) &snapshot, 1},
  {NULL, NULL, 0}
};

r_visible
void R_init_memtools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, r_callables, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
