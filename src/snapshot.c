#include <stdio.h>
#include <rlang.h>
#include "decl/snapshot-decl.h"

static sexp* snapshot_node_names = NULL;

#define DICT_SIZE_INIT 1024
#define STACK_SIZE_INIT 1024
#define STACK_GROWTH_FACTOR 1.5

struct snapshot_data {
  sexp* shelter;
  struct r_dict dict;
  int stack_size;
  int* p_sizes;
};


// [[ register() ]]
sexp* snapshot(sexp* x) {
  struct snapshot_data data = new_snapshot_data();
  KEEP(data.shelter);

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
  check_stack(data, depth);

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


// Snapshot data ----------------------------------------------------------

enum snapshot_shelter {
  SNAPSHOT_SHELTER_dict = 0,
  SNAPSHOT_SHELTER_sizes = 1
};

static
struct snapshot_data new_snapshot_data() {
  sexp* shelter = r_new_vector(r_type_list, 2);
  KEEP(shelter);

  struct r_dict dict = r_new_dict(DICT_SIZE_INIT);
  r_list_poke(shelter, SNAPSHOT_SHELTER_dict, dict.shelter);

  sexp* sizes = r_new_vector(r_type_integer, STACK_SIZE_INIT);
  r_list_poke(shelter, SNAPSHOT_SHELTER_sizes, sizes);

  struct snapshot_data data = (struct snapshot_data) {
    .shelter = shelter,
    .dict = dict,
    .stack_size = STACK_SIZE_INIT,
    .p_sizes = r_int_deref(sizes)
  };

  FREE(1);
  return data;
};

static
void check_stack(struct snapshot_data* data, int depth) {
  r_ssize size = data->stack_size;
  if (depth <= size) {
    return;
  }

  size = r_ssize_mult(size, STACK_GROWTH_FACTOR);

  sexp* sizes = r_list_get(data->shelter, SNAPSHOT_SHELTER_sizes);

  sizes = r_int_resize(sizes, size);
  r_list_poke(data->shelter, SNAPSHOT_SHELTER_sizes, sizes);
  data->p_sizes = r_int_deref(sizes);

  data->stack_size = size;
  return;
}


// Initialisation ---------------------------------------------------------

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
