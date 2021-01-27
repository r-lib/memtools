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
