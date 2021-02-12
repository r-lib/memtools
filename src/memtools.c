#include <rlang.h>

struct {
  sexp* id;
  sexp* type;
  sexp* self_size;
  sexp* parents;
  sexp* children;
} syms;

void init_library_memtools() {
  syms.id = r_sym("id");
  syms.type = r_sym("type");
  syms.self_size = r_sym("self_size");
  syms.parents = r_sym("parents");
  syms.children = r_sym("children");
}
