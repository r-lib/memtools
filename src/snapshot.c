#include <stdio.h>
#include <rlang.h>

static sexp* snapshot_node_names = NULL;

#define DICT_INIT_SIZE 1024
#define STACK_INIT_SIZE 1024
#define STACK_GROWTH_FACTOR 1.5

struct snapshot_stack {
  size_t retained_size;
  size_t retained_count;
};

struct snapshot_data {
  sexp* shelter;
  struct r_dict dict;
  int size;
  struct snapshot_stack v_stack[];
};

#include "decl/snapshot-decl.h"


// [[ register() ]]
sexp* snapshot(sexp* x) {
  struct snapshot_data* data = new_snapshot_data();
  KEEP(data->shelter);

  sexp_iterate(x, &snapshot_iterator, data);

  FREE(1);
  return r_null;
}

static
enum r_sexp_iterate snapshot_iterator(void* data,
                                      sexp* x,
                                      enum r_type type,
                                      int depth,
                                      sexp* parent,
                                      enum r_node_relation rel,
                                      r_ssize i,
                                      enum r_node_direction dir) {
  grow_stack(data, depth);
  return R_SEXP_ITERATE_next;
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
  SNAPSHOT_SHELTER_data,
  SNAPSHOT_SHELTER_dict
};

static
size_t snapshot_stack_size(size_t n) {
  return
    sizeof(struct snapshot_data) +
    sizeof(struct snapshot_stack) * n;
}

static
struct snapshot_data* new_snapshot_data() {
  sexp* shelter = KEEP(r_new_vector(r_type_list, 2));

  sexp* data_shelter = r_new_vector(r_type_raw, snapshot_stack_size(STACK_INIT_SIZE));
  r_list_poke(shelter, SNAPSHOT_SHELTER_data, data_shelter);

  struct snapshot_data* data = (struct snapshot_data*) r_raw_deref(shelter);
  data->shelter = shelter;
  data->size = 0;

  data->dict = r_new_dict(DICT_INIT_SIZE);
  r_list_poke(shelter, SNAPSHOT_SHELTER_dict, data->dict.shelter);

  FREE(1);
  return data;
}

static
void grow_stack(struct snapshot_data* data, int depth) {
  r_ssize size = data->size;
  if (depth <= size) {
    return;
  }

  size_t new_size = r_ssize_mult(size, STACK_GROWTH_FACTOR);
  new_size = snapshot_stack_size(new_size);

  sexp* data_shelter = r_list_get(data->shelter, SNAPSHOT_SHELTER_data);
  data_shelter = r_raw_resize(data_shelter, new_size);
  r_list_poke(data->shelter, SNAPSHOT_SHELTER_data, data_shelter);
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
