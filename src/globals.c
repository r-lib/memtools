#include <rlang.h>

struct {
  sexp* stash;
} attribs;

struct {
  sexp* children;
  sexp* dominated;
  sexp* dominator;
  sexp* gc_depth;
  sexp* id;
  sexp* mem_adj_list;
  sexp* mem_dict;
  sexp* mem_igraph;
  sexp* parents;
  sexp* retained_size;
  sexp* self_size;
  sexp* type;
} syms;


void init_library_globals() {
  attribs.stash = r_new_node(r_chr("memtools_stash"), r_null);
  r_preserve_global(attribs.stash);
  r_node_poke_tag(attribs.stash, r_syms_class);

  syms.children = r_sym("children");
  syms.dominated = r_sym("dominated");
  syms.dominator = r_sym("dominator");
  syms.gc_depth = r_sym("gc_depth");
  syms.id = r_sym("id");
  syms.mem_dict = r_sym("mem_dict");
  syms.mem_igraph = r_sym("mem_igraph");
  syms.mem_adj_list = r_sym("mem_adj_list");
  syms.parents = r_sym("parents");
  syms.retained_size = r_sym("retained_size");
  syms.self_size = r_sym("self_size");
  syms.type = r_sym("type");
}
