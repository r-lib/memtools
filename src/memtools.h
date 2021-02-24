#ifndef MEMTOOLS_H
#define MEMTOOLS_H


extern struct {
  sexp* id;
  sexp* type;
  sexp* self_size;
  sexp* parents;
  sexp* children;
  sexp* dominator;
  sexp* dominated;
  sexp* gc_depth;
} syms;


#endif
