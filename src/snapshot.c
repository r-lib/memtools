#include <stdio.h>
#include <rlang.h>

#include "arrow.h"
#include "dominance.h"
#include "memtools.h"
#include "node.h"
#include "utils.h"

#define DICT_INIT_SIZE 1024

#define STACK_INIT_SIZE 1024
#define STACK_GROWTH_FACTOR 2

#define NODES_PARENTS_INIT_SIZE 4


enum snapshot_df_locs {
  SNAPSHOT_DF_LOCS_id = 0,
  SNAPSHOT_DF_LOCS_type,
  SNAPSHOT_DF_LOCS_node,
  SNAPSHOT_DF_LOCS_n_parents,
  SNAPSHOT_DF_LOCS_n_children,
  SNAPSHOT_DF_LOCS_n_dominated,
  SNAPSHOT_DF_LOCS_retained_size,
  SNAPSHOT_DF_LOCS_gc_depth,
  SNAPSHOT_DF_SIZE
};
static
const char* snapshot_df_names_c_strings[SNAPSHOT_DF_SIZE] = {
  "id",
  "type",
  "node",
  "n_parents",
  "n_children",
  "n_dominated",
  "retained_size",
  "gc_depth"
};
static
const enum r_type snapshot_df_types[SNAPSHOT_DF_SIZE] = {
  r_type_character,
  r_type_character,
  r_type_list,
  r_type_integer,
  r_type_integer,
  r_type_integer,
  r_type_double,
  r_type_integer
};
static
sexp* snapshot_df_names = NULL;

struct snapshot_state {
  sexp* shelter;
  struct r_dict* p_dict;
  struct r_dyn_array* p_node_arr;
  struct r_dyn_list_of* p_parents_lof;
};

struct dom_tree_info {
  int depth;
  r_ssize retained_size;
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

  struct r_pair_ptr_ssize* vv_parents = p_state->p_parents_lof->v_data;
  struct node* v_nodes = r_arr_ptr_front(p_node_arr);

  // The heap snapshot is a multigraph, it has duplicated edges
  // because there are several ways in which a node can point to
  // another. Remove all duplicated edges before passing the adjacency
  // list to the dominance algorithm which only works on simple
  // graphs.
  for (int i = 0; i < n_rows; ++i) {
    int* start = vv_parents[i].ptr;
    int* end = r_int_unique0(start, vv_parents[i].size);
    vv_parents[i].size = start - end;
  }

  struct dom_info* v_dom;
  KEEP(node_dominators0(vv_parents, n_rows, &v_dom));

  
  struct r_dyn_list_of* p_dominated = r_new_dyn_list_of(r_type_integer, n_rows, 3);
  KEEP(p_dominated->shelter);

  for (int i = 0; i < n_rows; ++i) {
    r_lof_push_back(p_dominated);
  }
  for (int i = n_rows - 1; i >= 0; --i) {
    int idom = v_dom[i].idom;
    if (idom >= 0) {
      r_lof_arr_push_back(p_dominated, idom, &i);
    }
  }

  struct r_pair_ptr_ssize* vv_dominated = p_dominated->v_data;

  struct dom_tree_info* v_dom_tree_info;
  KEEP(dominance_info(vv_dominated, v_nodes, n_rows, &v_dom_tree_info));


  sexp* id_col = r_list_get(df, SNAPSHOT_DF_LOCS_id);
  sexp* type_col = r_list_get(df, SNAPSHOT_DF_LOCS_type);
  sexp* node_col = r_list_get(df, SNAPSHOT_DF_LOCS_node);
  sexp* n_parents_col = r_list_get(df, SNAPSHOT_DF_LOCS_n_parents);
  sexp* n_children_col = r_list_get(df, SNAPSHOT_DF_LOCS_n_children);
  sexp* n_dominated_col = r_list_get(df, SNAPSHOT_DF_LOCS_n_dominated);
  sexp* retained_size_col = r_list_get(df, SNAPSHOT_DF_LOCS_retained_size);
  sexp* gc_depth_col = r_list_get(df, SNAPSHOT_DF_LOCS_gc_depth);

  init_attrib_bytes(retained_size_col);

  sexp* const * v_node_col = r_list_deref_const(node_col);

  for (int i = 0; i < n_rows; ++i) {
    struct node node = v_nodes[i];

    sexp* node_type_str = KEEP(r_type_as_string(node.type));
    sexp* parents_list = KEEP(r_arr_unwrap(node.p_parents_list));
    sexp* children_list = KEEP(r_arr_unwrap(node.p_children_list));

    sexp* dominator_node;
    int idom = v_dom[i].idom;
    if (idom < 0) {
      dominator_node = r_null;
    } else {
      dominator_node = v_node_col[idom];
    }

    r_ssize retained_size = v_dom_tree_info[i].retained_size;

    sexp* node_env = node.env;
    r_env_poke(node_env, syms.id, r_str_as_character(node.id));
    r_env_poke(node_env, syms.type, r_str_as_character(node_type_str));
    r_env_poke(node_env, syms.self_size, r_len(node.self_size));
    r_env_poke(node_env, syms.parents, parents_list);
    r_env_poke(node_env, syms.children, children_list);
    r_env_poke(node_env, syms.dominator, dominator_node);
    r_env_poke(node_env, syms.gc_depth, r_len(v_dom_tree_info[i].depth));
    r_env_poke(node_env, syms.retained_size, r_len(retained_size));

    r_chr_poke(id_col, i, node.id);
    r_chr_poke(type_col, i, node_type_str);
    r_list_poke(node_col, i, node_env);
    r_int_poke(n_parents_col, i, r_length(parents_list));
    r_int_poke(n_children_col, i, r_length(children_list));
    r_int_poke(n_dominated_col, i, vv_dominated[i].size);
    r_dbl_poke(retained_size_col, i, retained_size);
    r_int_poke(gc_depth_col, i, v_dom_tree_info[i].depth);

    FREE(3);
  }


  // This must come in a second pass so that `v_node_col` is populated
  for (int i = 0; i < n_rows; ++i) {
    int n_dominated = vv_dominated[i].size;
    int* v_dominated = vv_dominated[i].ptr;

    sexp* dominated = r_new_list(n_dominated);

    for (int j = 0; j < n_dominated; ++j) {
      r_list_poke(dominated, j, v_node_col[v_dominated[j]]);
    }

    sexp* node_env = v_nodes[i].env;
    r_env_poke(node_env, syms.dominated, dominated);
  }

  r_attrib_push(df, syms.mem_dict, p_state->p_dict->shelter);

  FREE(5);
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

  int loc = p_cached_node ? p_cached_node->depth_first_loc : p_state->p_node_arr->count;
  int parent_loc = p_parent_node ? p_parent_node->depth_first_loc : -1;

  if (p_cached_node) {
    if (dir != R_NODE_DIRECTION_outgoing) {
      sexp* arrow = KEEP(new_arrow(parent_node_env,
                                   p_cached_node->env,
                                   depth,
                                   rel,
                                   i));
      r_lof_arr_push_back(p_state->p_parents_lof, loc, &parent_loc);
      r_arr_push_back(p_cached_node->p_parents_list, &arrow);
      r_arr_push_back(p_parent_node->p_children_list, &arrow);
      FREE(1);
    }

    return R_SEXP_ITERATE_skip;
  }

  r_lof_push_back(p_state->p_parents_lof);

  struct node node;
  init_node(&node, x, type, loc);
  KEEP_WHILE(node.shelter, r_dict_put(p_state->p_dict, x, node.shelter));
  r_arr_push_back(p_state->p_node_arr, &node);

  // Only NULL when parent is root node
  if (p_parent_node) {
    sexp* arrow = new_arrow(parent_node_env, node.env, depth, rel, i);
    r_lof_arr_push_back(p_state->p_parents_lof, loc, &parent_loc);
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
  SHELTER_SNAPSHOT_parents,
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

  struct r_dyn_list_of* p_parents_lof = r_new_dyn_list_of(r_type_integer,
                                                          NODES_INIT_SIZE,
                                                          NODES_PARENTS_INIT_SIZE);
  r_list_poke(shelter, SHELTER_SNAPSHOT_parents, p_parents_lof->shelter);

  struct snapshot_state* state = r_raw_deref(state_shelter);
  *state = (struct snapshot_state) {
    .shelter = shelter,
    .p_node_arr = p_node_arr,
    .p_dict = p_dict,
    .p_parents_lof = p_parents_lof
  };

  FREE(1);
  return state;
}

static
sexp* dominance_info(const struct r_pair_ptr_ssize* vv_dominated,
                     const struct node* v_nodes,
                     int n_nodes,
                     struct dom_tree_info** out_v_info) {
  sexp* dom_tree = KEEP(r_new_raw(sizeof(struct dom_tree_info) * n_nodes));
  struct dom_tree_info* v_info = r_raw_deref(dom_tree);

  dominance_info_rec(0, 0, vv_dominated, v_nodes, v_info);

  FREE(1);
  *out_v_info = v_info;
  return dom_tree;
}

static
r_ssize dominance_info_rec(int i,
                           int depth,
                           const struct r_pair_ptr_ssize* vv_dominated,
                           const struct node* v_nodes,
                           struct dom_tree_info* v_info) {
  int* v_dominated = vv_dominated[i].ptr;
  int n_dominated = vv_dominated[i].size;

  ++depth;

  r_ssize retained_size = v_nodes[i].self_size;

  for (int j = 0; j < n_dominated; ++j) {
    retained_size += dominance_info_rec(v_dominated[j],
                                        depth,
                                        vv_dominated,
                                        v_nodes,
                                        v_info);
  }

  v_info[i] = (struct dom_tree_info) {
    .depth = depth,
    .retained_size = retained_size
  };

  return retained_size;
}


void init_library_snapshot() {
  size_t df_names_size = R_ARR_SIZEOF(snapshot_df_names_c_strings);
  size_t df_types_size = R_ARR_SIZEOF(snapshot_df_types);
  RLANG_ASSERT(df_names_size == df_types_size);

  snapshot_df_names = r_chr_n(snapshot_df_names_c_strings, df_names_size);
  r_preserve_global(snapshot_df_names);
}
