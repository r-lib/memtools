#include <stdio.h>
#include <rlang.h>
#include "decl/snapshot-decl.h"

static sexp* snapshot_node_names = NULL;

struct snapshot_data {
  struct r_dict dict;
};


sexp* snapshot(sexp* x) {
  struct r_dict dict = r_new_dict(1024);
  KEEP(dict.shelter);

  struct snapshot_data data = (struct snapshot_data) {
    .dict = dict
  };

  FREE(1);
  return r_null;
}

static
bool snapshot_iterator(void* data,
                       sexp* x,
                       enum r_type type,
                       int depth,
                       sexp* parent,
                       enum r_node_relation rel,
                       r_ssize i,
                       bool on_return) {
  sexp* id = KEEP(r_sexp_address(x));
  sexp* node = KEEP(new_snapshot_node(id, type));

  r_ssize size = obj_size(x);

  if (on_return) {
    ;
  }

  FREE(2);
  return true;
}

static
sexp* new_snapshot_node(sexp* id, enum r_type type) {
  sexp* node = KEEP(r_new_vector(r_type_list, 2));

  r_attrib_push(node, r_syms_names, snapshot_node_names);

  r_list_poke(node, 0, r_str_as_character(id));
  r_list_poke(node, 1, r_type_as_character(type));
  r_list_poke(node, 2, r_int(0));

  FREE(1);
  return node;
}

static
r_ssize obj_size(sexp* x) {
  // TODO
  return 10;
}


void init_snapshot() {
  const char* snapshot_node_names_code = "c('id', 'type', 'retained_count')";
  snapshot_node_names = r_parse_eval(snapshot_node_names_code, r_base_env);
  r_mark_precious(snapshot_node_names);
}

static
const R_CallMethodDef r_callables[] = {
  {"ptr_snapshot",           (r_void_fn) &snapshot, 1},
  {NULL, NULL, 0}
};

r_visible
void R_init_memtools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, r_callables, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  r_init_library();
  init_snapshot();
}
