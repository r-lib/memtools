#ifndef MEMTOOLS_H
#define MEMTOOLS_H


extern struct {
  sexp* id;
  sexp* type;
  sexp* self_size;
  sexp* parents;
  sexp* children;
  sexp* dominator;
} syms;


#endif
