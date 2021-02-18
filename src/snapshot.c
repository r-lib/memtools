#include <stdio.h>
#include <rlang.h>
#include "arrow.h"
#include "memtools.h"
#include "node.h"

#define DICT_INIT_SIZE 1024

#define STACK_INIT_SIZE 1024
#define STACK_GROWTH_FACTOR 2


enum snapshot_df_locs {
  SNAPSHOT_DF_LOCS_id = 0,
  SNAPSHOT_DF_LOCS_type,
  SNAPSHOT_DF_LOCS_node,
  SNAPSHOT_DF_LOCS_parents,
  SNAPSHOT_DF_LOCS_children,
  SNAPSHOT_DF_SIZE
};
static
const char* snapshot_df_names_c_strings[SNAPSHOT_DF_SIZE] = {
  "id",
  "type",
  "node",
  "parents",
  "children",
};
static
const enum r_type snapshot_df_types[SNAPSHOT_DF_SIZE] = {
  r_type_character,
  r_type_character,
  r_type_list,
  r_type_list,
  r_type_list,
};
static
sexp* snapshot_df_names = NULL;

struct snapshot_state {
  sexp* shelter;
  struct r_dict* p_dict;
  struct r_dyn_array* p_node_arr;
};

#include "decl/snapshot-decl.h"


// [[ register() ]]
sexp* snapshot(sexp* x) {
  struct snapshot_state* p_state = new_snapshot_state();
  KEEP(p_state->shelter);

  sexp_iterate(x, &snapshot_iterator, p_state);

  struct r_dyn_array* p_node_arr = p_state->p_node_arr;

  // Transform to data frame
  r_ssize n_rows = p_node_arr->count;
  sexp* df = KEEP(r_alloc_df_list(n_rows,
                                  snapshot_df_names,
                                  snapshot_df_types,
                                  SNAPSHOT_DF_SIZE));
  r_init_tibble(df, n_rows);

  sexp* id_col = r_list_get(df, SNAPSHOT_DF_LOCS_id);
  sexp* type_col = r_list_get(df, SNAPSHOT_DF_LOCS_type);
  sexp* node_col = r_list_get(df, SNAPSHOT_DF_LOCS_node);
  sexp* parents_col = r_list_get(df, SNAPSHOT_DF_LOCS_parents);
  sexp* children_col = r_list_get(df, SNAPSHOT_DF_LOCS_children);

  struct node* v_nodes = r_arr_ptr_front(p_node_arr);

  for (r_ssize i = 0; i < n_rows; ++i) {
    struct node node = v_nodes[i];

    sexp* node_type_str = KEEP(r_type_as_string(node.type));
    sexp* parents_list = KEEP(r_arr_unwrap(node.p_parents_list));
    sexp* children_list = KEEP(r_arr_unwrap(node.p_children_list));

    sexp* node_env = node.env;
    r_env_poke(node_env, syms.id, r_str_as_character(node.id));
    r_env_poke(node_env, syms.type, r_str_as_character(node_type_str));
    r_env_poke(node_env, syms.self_size, r_len(node.self_size));
    r_env_poke(node_env, syms.parents, parents_list);
    r_env_poke(node_env, syms.children, children_list);

    r_chr_poke(id_col, i, node.id);
    r_chr_poke(type_col, i, node_type_str);
    r_list_poke(node_col, i, node_env);
    r_list_poke(parents_col, i, parents_list);
    r_list_poke(children_col, i, children_list);

    FREE(3);
  }

  FREE(2);
  return df;
}

static
enum r_sexp_iterate snapshot_iterator(void* payload,
                                      sexp* x,
                                      enum r_type type,
                                      int depth,
                                      sexp* parent,
                                      enum r_node_relation rel,
                                      r_ssize i,
                                      enum r_node_direction dir) {
  struct snapshot_state* p_state = (struct snapshot_state*) payload;

  if (type == r_type_null) {
    return R_SEXP_ITERATE_next;
  }

  // The parent node is `NULL` if `x` is the root
  struct node* p_parent_node = get_cached_parent_node(p_state, parent);
  sexp* parent_node_env = p_parent_node ? p_parent_node->env : r_null;

  // We might have already visited `x`
  struct node* p_cached_node = get_cached_node(p_state, x);

  if (p_cached_node) {
    if (dir != R_NODE_DIRECTION_outgoing) {
      sexp* arrow = KEEP(new_arrow(parent_node_env,
                                   p_cached_node->env,
                                   depth,
                                   rel,
                                   i));
      r_dbl_push_back(p_cached_node->p_parents_locs, p_parent_node->loc);
      r_arr_push_back(p_cached_node->p_parents_list, &arrow);
      r_arr_push_back(p_parent_node->p_children_list, &arrow);
      FREE(1);
    }

    return R_SEXP_ITERATE_skip;
  }

  struct node node;
  init_node(&node, x, type, p_state->p_node_arr->count);
  KEEP_WHILE(node.shelter, r_dict_put(p_state->p_dict, x, node.shelter));
  r_arr_push_back(p_state->p_node_arr, &node);

  // Only NULL when parent is root node
  if (p_parent_node) {
    sexp* arrow = new_arrow(parent_node_env, node.env, depth, rel, i);
    r_dbl_push_back(node.p_parents_locs, p_parent_node->loc);
    r_arr_push_back(node.p_parents_list, &arrow);
    r_arr_push_back(p_parent_node->p_children_list, &arrow);
  }


  // Skip bindings of the global environment as they will contain
  // objects from the debugging session, including memory snapshots.
  // TODO: Traverse global env manually to collect hidden symbols
  // starting with a dot.
  if (parent == r_global_env && rel != R_NODE_RELATION_environment_enclos) {
    return R_SEXP_ITERATE_skip;
  }

  return R_SEXP_ITERATE_next;
}

static
struct node* get_cached_node(struct snapshot_state* p_state,
                                      sexp* x) {
  sexp* cached = r_dict_get0(p_state->p_dict, x);
  if (cached) {
    int i = r_int_get(r_list_get(cached, SHELTER_NODE_location), 0);
    return r_arr_ptr(p_state->p_node_arr, i);
  } else {
    return NULL;
  }
}
static
struct node* get_cached_parent_node(struct snapshot_state* p_state,
                                             sexp* parent) {
  static sexp* last_sexp = NULL;
  static struct node* p_last_node = NULL;

  if (parent == last_sexp) {
    return p_last_node;
  }

  last_sexp = parent;
  p_last_node = get_cached_node(p_state, parent);

  return p_last_node;
}


// Snapshot data ----------------------------------------------------------

enum shelter_snapshot {
  SHELTER_SNAPSHOT_state,
  SHELTER_SNAPSHOT_nodes,
  SHELTER_SNAPSHOT_dict,
  SHELTER_SNAPSHOT_SIZE
};

static
struct snapshot_state* new_snapshot_state() {
  sexp* shelter = KEEP(r_new_vector(r_type_list, SHELTER_SNAPSHOT_SIZE));

  sexp* state_shelter = r_new_vector(r_type_raw, sizeof(struct snapshot_state));
  r_list_poke(shelter, SHELTER_SNAPSHOT_state, state_shelter);

  struct r_dyn_array* p_node_arr = r_new_dyn_array(sizeof(struct node), NODES_INIT_SIZE);
  r_list_poke(shelter, SHELTER_SNAPSHOT_nodes, p_node_arr->shelter);

  struct r_dict* p_dict = r_new_dict(DICT_INIT_SIZE);
  r_list_poke(shelter, SHELTER_SNAPSHOT_dict, p_dict->shelter);

  struct snapshot_state* state = (struct snapshot_state*) r_raw_deref(state_shelter);
  state->shelter = shelter;
  state->p_node_arr = p_node_arr;
  state->p_dict = p_dict;

  FREE(1);
  return state;
}


void init_library_snapshot() {
  size_t df_names_size = R_ARR_SIZEOF(snapshot_df_names_c_strings);
  size_t df_types_size = R_ARR_SIZEOF(snapshot_df_types);
  RLANG_ASSERT(df_names_size == df_types_size);

  snapshot_df_names = r_chr_n(snapshot_df_names_c_strings, df_names_size);
  r_preserve_global(snapshot_df_names);
}
