#include <stdio.h>
#include <rlang.h>

static sexp* snapshot_node_names = NULL;

#define DICT_INIT_SIZE 1024

#define STACK_INIT_SIZE 1024
#define STACK_GROWTH_FACTOR 1.5

#define NODES_INIT_SIZE 65536
#define NODES_GROWTH_FACTOR 2

#define ARROWS_INIT_SIZE 5
#define ARROWS_GROWTH_FACTOR 2

struct snapshot_data {
  r_ssize retained_size;
  r_ssize retained_count;
};
struct snapshot_data_stack {
  sexp* shelter;
  r_ssize size;
  struct snapshot_data v_stack[];
};

struct snapshot_node {
  sexp* id;
  enum r_type type;
  r_ssize size;
  sexp* arrow_list;
  int arrow_list_n;
  r_ssize retained_count;
  r_ssize retained_size;
};
struct snapshot_node_stack {
  sexp* shelter;
  sexp* shelter_extra;
  r_ssize size;
  r_ssize n;
  struct snapshot_node v_nodes[];
};

struct snapshot_state {
  sexp* shelter;
  struct r_dict dict;
  struct snapshot_node_stack* p_node_stack;
  struct snapshot_data_stack* p_data_stack;
};

#include "decl/snapshot-decl.h"


// [[ register() ]]
sexp* snapshot(sexp* x) {
  struct snapshot_state* p_state = new_snapshot_state();
  KEEP(p_state->shelter);

  sexp_iterate(x, &snapshot_iterator, p_state);

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
  struct snapshot_state* p_state = (struct snapshot_state*) data;
  data_stack_grow(p_state->p_data_stack, depth);

  if (dir == R_NODE_DIRECTION_outgoing) {
    return R_SEXP_ITERATE_next;
    r_abort("TODO: Carry");
  }

  sexp* id = KEEP(r_sexp_address(x));
  sexp* arrow_list = KEEP(new_arrow_list(x));

  struct snapshot_node node = {
    .id = id,
    .type = type,
    .size = 10, // TODO: Actual size computation
    .arrow_list = arrow_list
  };

  // TODO: Update arrows of parent as well?
  sexp* arrow = KEEP(new_arrow(id, depth, parent, rel, i));
  node_push_arrow(&node, arrow);

  node_stack_push(p_state->p_node_stack, node);

  FREE(3);
  return R_SEXP_ITERATE_next;
}


// Snapshot data ----------------------------------------------------------

enum shelter_snapshot {
  SHELTER_SNAPSHOT_data,
  SHELTER_SNAPSHOT_stack,
  SHELTER_SNAPSHOT_nodes,
  SHELTER_SNAPSHOT_dict,
  SHELTER_SNAPSHOT_total_size
};

static
struct snapshot_state* new_snapshot_state() {
  sexp* shelter = KEEP(r_new_vector(r_type_list, SHELTER_SNAPSHOT_total_size));


  sexp* state_shelter = r_new_vector(r_type_raw, sizeof(struct snapshot_state));
  r_list_poke(shelter, SHELTER_SNAPSHOT_data, state_shelter);

  sexp* data_stack_shelter = r_pairlist(r_new_vector(r_type_raw, data_stack_size(STACK_INIT_SIZE)));
  r_list_poke(shelter, SHELTER_SNAPSHOT_stack, data_stack_shelter);


  sexp* node_stack_shelter_meta = r_new_vector(r_type_list, 2);
  r_list_poke(shelter, SHELTER_SNAPSHOT_nodes, node_stack_shelter_meta);

  sexp* node_stack_shelter = r_new_vector(r_type_raw, node_stack_size(NODES_INIT_SIZE));
  r_list_poke(node_stack_shelter_meta, 0, node_stack_shelter);

  sexp* node_stack_shelter_extra = r_pairlist(r_new_vector(r_type_list, NODES_INIT_SIZE * 2));
  r_list_poke(node_stack_shelter_meta, 1, node_stack_shelter_extra);


  struct r_dict dict = r_new_dict(DICT_INIT_SIZE);
  r_list_poke(shelter, SHELTER_SNAPSHOT_dict, dict.shelter);


  struct snapshot_data_stack* p_data_stack = (struct snapshot_data_stack*) r_raw_deref(r_node_car(data_stack_shelter));
  p_data_stack->shelter = data_stack_shelter;
  p_data_stack->size = 0;

  struct snapshot_node_stack* p_node_stack = (struct snapshot_node_stack*) r_raw_deref(node_stack_shelter);
  p_node_stack->shelter = node_stack_shelter_meta;
  p_node_stack->shelter_extra = node_stack_shelter_extra;
  p_node_stack->size = 0;
  p_node_stack->n = 0;

  struct snapshot_state* state = (struct snapshot_state*) r_raw_deref(state_shelter);
  state->shelter = shelter;
  state->p_node_stack = p_node_stack;
  state->p_data_stack = p_data_stack;
  state->dict = dict;

  FREE(1);
  return state;
}

static
size_t data_stack_size(size_t n) {
  return
    sizeof(struct snapshot_data_stack) +
    sizeof(struct snapshot_data) * n;
}
static
size_t node_stack_size(size_t n) {
  return
    sizeof(struct snapshot_node_stack) +
    sizeof(struct snapshot_node) * n;
}

static inline
void data_stack_grow(struct snapshot_data_stack* x, r_ssize i) {
  r_ssize size = x->size;
  if (i <= size) {
    return;
  }

  size_t new_size = r_ssize_mult(size, STACK_GROWTH_FACTOR);
  new_size = data_stack_size(new_size);

  sexp* shelter = r_raw_resize(r_node_car(x->shelter), new_size);
  r_node_poke_car(x->shelter, shelter);
}

static inline
void node_stack_grow(struct snapshot_node_stack* x, r_ssize i) {
  r_ssize size = x->size;
  if (i <= size) {
    return;
  }

  size_t new_size = r_ssize_mult(size, NODES_GROWTH_FACTOR);
  new_size = node_stack_size(new_size);

  size_t new_size_extra = r_ssize_mult(new_size, 2);

  sexp* shelter = r_raw_resize(r_list_get(x->shelter, 0), new_size);
  r_list_poke(x->shelter, 0, shelter);

  sexp* shelter_extra = r_list_resize(x->shelter_extra, new_size_extra);
  r_list_poke(x->shelter, 1, shelter);
  x->shelter_extra = shelter_extra;
}

static
void node_stack_push(struct snapshot_node_stack* p_node_stack,
                     struct snapshot_node node) {
  r_ssize n = p_node_stack->n + 1;
  node_stack_grow(p_node_stack, n);

  p_node_stack->n = n;
  p_node_stack->v_nodes[n] = node;

  // Protect `sexp*` members of the snapshot node
  r_list_poke(p_node_stack->shelter_extra, n, node.id);
  r_list_poke(p_node_stack->shelter_extra, n + 1, node.arrow_list);
}


// Nodes and arrows -------------------------------------------------------

static
sexp* new_arrow_list(sexp* x) {
  // Make space for a few arrows per node. The arrow lists are
  // compacted later on.
  r_ssize n = ARROWS_INIT_SIZE;

  switch (r_typeof(x)) {
  default:
    break;
  case r_type_character:
  case r_type_expression:
  case r_type_list:
    n += r_length(x);
    break;
  }

  return r_new_vector(r_type_list, n), r_null;
}

static
sexp* new_arrow(sexp* id,
                int depth,
                sexp* parent,
                enum r_node_relation rel,
                r_ssize i) {
  sexp* arrow = KEEP(r_new_vector(r_type_list, 5));

  sexp* to_from = r_new_vector(r_type_character, 2);
  r_list_poke(arrow, 0, to_from);

  r_chr_poke(to_from, 0, r_sexp_address(parent));
  r_chr_poke(to_from, 1, id);

  // TODO: Add rest of data

  FREE(1);
  return arrow;
}

static
void node_push_arrow(struct snapshot_node* node, sexp* arrow) {
  sexp* arrow_list = node->arrow_list;

  if (node->arrow_list_n == r_length(arrow_list)) {
    r_abort("TODO: Grow arrow list");
  }

  r_list_poke(arrow_list, node->arrow_list_n++, arrow);
}


// Initialisation ---------------------------------------------------------

sexp* init_memtools() {
  const char* snapshot_node_names_code = "c('id', 'type', 'parent')";
  snapshot_node_names = r_parse_eval(snapshot_node_names_code, r_base_env);
  r_preserve_global(snapshot_node_names);
  return r_null;
}

static
const R_CallMethodDef r_callables[] = {
  {"c_ptr_snapshot",               (r_void_fn) &snapshot, 1},
  {"c_ptr_init_library",           (r_void_fn) &r_init_library, 1},
  {"c_ptr_init_memtools",          (r_void_fn) &init_memtools, 1},
  {NULL, NULL, 0}
};

r_visible
void R_init_memtools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, r_callables, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
