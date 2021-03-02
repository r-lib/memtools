#ifndef MEMTOOLS_H
#define MEMTOOLS_H


struct {
  sexp* stash;
} attribs;

extern struct {
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

static inline
bool is_mem_stash(sexp* x) {
  return r_attrib(x) == attribs.stash;
}


#endif
