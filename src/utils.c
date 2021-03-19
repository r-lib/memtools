#include <stdlib.h>
#include <rlang.h>
#include <errno.h>


// [[ register() ]]
sexp* addr_deref(sexp* addr) {
  if (!r_is_string(addr)) {
    r_abort("`addr` must be a string.");
  }

  const char* addr_c_str = r_chr_get_c_string(addr, 0);

  errno = 0;
  unsigned long long addr_ull = strtoull(addr_c_str, NULL, 16);
  if (errno || !addr_ull) {
    r_abort("Can't convert address to pointer.");
  }
  
  sexp* obj = (sexp*) addr_ull;

  return obj;
}

sexp* attribs_bytes = NULL;

void init_attrib_bytes(sexp* x) {
  if (r_typeof(x) != R_TYPE_double) {
    r_abort("Can't initialise an object of type `%s` as `rlang_bytes`.",
            r_type_as_c_string(r_typeof(x)));
  }

  r_poke_attrib(x, attribs_bytes);
  r_mark_object(x);
}
sexp* bytes(r_ssize x) {
  sexp* out = KEEP(r_dbl(x));
  init_attrib_bytes(out);
  FREE(1);
  return out;
}

// [[ register() ]]
sexp* ffi_root_cpp11() {
  sexp* out = r_peek_option("cpp11_preserve_xptr");
  if (r_typeof(out) != R_TYPE_pointer) {
    return r_null;
  }

  void* addr = R_ExternalPtrAddr(out);
  if (!addr) {
    return r_null;
  }

  return (sexp*) addr;
}
// [[ register() ]]
sexp* ffi_root_ns_registry() {
  return R_NamespaceRegistry;
}


// [[ register() ]]
sexp* ffi_list_poke(sexp* x, sexp* i, sexp* value) {
  r_list_poke(x, r_as_ssize(i), value);
  return r_null;
}

void init_library_utils() {
  const char* v_bytes_class[] = { "bench_bytes", "numeric" };
  sexp* bytes_class = KEEP(r_chr_n(v_bytes_class, R_ARR_SIZEOF(v_bytes_class)));
  attribs_bytes = r_pairlist(bytes_class);
  r_preserve_global(attribs_bytes);
  r_node_poke_tag(attribs_bytes, r_syms.class);
  FREE(1);
}
