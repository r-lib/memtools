#include <rlang.h>

struct {
  sexp* id;
  sexp* type;
  sexp* self_size;
  sexp* parents;
  sexp* children;
  sexp* dominator;
  sexp* dominated;
  sexp* gc_depth;
  sexp* retained_size;
} syms;

void init_library_memtools() {
  syms.id = r_sym("id");
  syms.type = r_sym("type");
  syms.self_size = r_sym("self_size");
  syms.parents = r_sym("parents");
  syms.children = r_sym("children");
  syms.dominator = r_sym("dominator");
  syms.dominated = r_sym("dominated");
  syms.gc_depth = r_sym("gc_depth");
  syms.retained_size = r_sym("retained_size");
}
