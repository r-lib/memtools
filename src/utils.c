#include <stdlib.h>
#include <rlang.h>

// [[ register() ]]
sexp* addr_deref(sexp* addr) {
  if (!r_is_string(addr)) {
    r_abort("`addr` must be a string.");
  }

  const char* addr_c_str = r_chr_get_c_string(addr, 0);
  sexp* obj = (sexp*) strtoul(addr_c_str, NULL, 0);

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
sexp* ffi_list_poke(sexp* x, sexp* i, sexp* value) {
  r_list_poke(x, r_as_ssize(i), value);
  return r_null;
}

void init_library_utils() {
  const char* v_bytes_class[] = { "bench_bytes", "numeric" };
  sexp* bytes_class = KEEP(r_chr_n(v_bytes_class, R_ARR_SIZEOF(v_bytes_class)));
  attribs_bytes = r_pairlist(bytes_class);
  r_preserve_global(attribs_bytes);
  r_node_poke_tag(attribs_bytes, r_syms_class);
  FREE(1);
}
