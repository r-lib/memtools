#include <rlang.h>


struct {
  sexp* stash;
} attribs;

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
  sexp* mem_dict;
} syms;


// [[ register() ]]
sexp* ffi_new_stash(sexp* stash) {
  r_poke_attrib(stash, attribs.stash);
  return stash;
}


void init_library_memtools() {
  attribs.stash = r_new_node(r_chr("memtools_stash"), r_null);
  r_preserve_global(attribs.stash);
  r_node_poke_tag(attribs.stash, r_syms_class);

  syms.id = r_sym("id");
  syms.type = r_sym("type");
  syms.self_size = r_sym("self_size");
  syms.parents = r_sym("parents");
  syms.children = r_sym("children");
  syms.dominator = r_sym("dominator");
  syms.dominated = r_sym("dominated");
  syms.gc_depth = r_sym("gc_depth");
  syms.retained_size = r_sym("retained_size");
  syms.mem_dict = r_sym("mem_dict");
}
