#include <rlang.h>

// [[ register() ]]
sexp* init_memtools(sexp* ns, sexp* node_size, sexp* vec_size) {
  void init_library_arrow();
  void init_library_memtools();
  void init_library_node();
  void init_library_size();
  void init_library_snapshot();
  void init_library_utils();

  init_library_arrow();
  init_library_memtools();
  init_library_node();
  init_library_size(node_size, vec_size);
  init_library_snapshot();
  init_library_utils();

  return r_null;
}

sexp* r_sexp_self_size(sexp* addr);
sexp* addr_deref(sexp* addr);
sexp* snapshot(sexp* x);
sexp* c_ptr_node_size(sexp* x);
sexp* ffi_new_stash(sexp* stash);
sexp* ffi_node_dominators(sexp* parents);
sexp* ffi_root_cpp11();

static
const R_CallMethodDef r_callables[] = {
  {"c_ptr_init_library",           (r_void_fn) &r_init_library, 1},
  {"c_ptr_init_memtools",          (r_void_fn) &init_memtools, 3},
  {"c_ptr_addr_deref",             (r_void_fn) &addr_deref, 1},
  {"c_ptr_sexp_self_size",         (r_void_fn) &r_sexp_self_size, 1},
  {"c_ptr_snapshot",               (r_void_fn) &snapshot, 1},
  {"c_ptr_mem_node_size",          (r_void_fn) &c_ptr_node_size, 1},
  {"ffi_new_stash",                (r_void_fn) &ffi_new_stash, 1},
  {"ffi_node_dominators",          (r_void_fn) &ffi_node_dominators, 1},
  {"ffi_root_cpp11",               (r_void_fn) &ffi_root_cpp11, 0},
  {NULL, NULL, 0}
};

r_visible
void R_init_memtools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, r_callables, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
