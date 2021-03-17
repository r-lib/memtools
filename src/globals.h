#ifndef MEMTOOLS_GLOBALS_H
#define MEMTOOLS_GLOBALS_H


struct {
  sexp* stash;
} attribs;

extern struct {
  sexp* children;
  sexp* dominated;
  sexp* dominator;
  sexp* gc_depth;
  sexp* id;
  sexp* mem_adj_list;
  sexp* mem_dict;
  sexp* mem_id;
  sexp* mem_igraph;
  sexp* mem_node;
  sexp* parents;
  sexp* retained_size;
  sexp* self_size;
  sexp* type;
} syms;


#endif
