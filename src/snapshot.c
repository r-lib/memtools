#include <stdio.h>
#include <rlang.h>
#include "size.h"

#define DICT_INIT_SIZE 1024

#define STACK_INIT_SIZE 1024
#define STACK_GROWTH_FACTOR 2

#define NODES_INIT_SIZE 65536
#define NODES_GROWTH_FACTOR 2

#define ARROWS_INIT_SIZE 2
#define ARROWS_GROWTH_FACTOR 2

struct {
  sexp* id;
  sexp* type;
  sexp* self_size;
  sexp* parents;
  sexp* children;
} syms;

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

struct node {
  sexp* env;
  sexp* id;
  enum r_type type;
  r_ssize self_size;
  struct r_dyn_array* p_parents_list;
  struct r_dyn_array* p_children_list;
};
enum shelter_node {
  SHELTER_NODE_location = 0,
  SHELTER_NODE_id,
  SHELTER_NODE_env,
  SHELTER_NODE_parents_dict,
  SHELTER_NODE_children_dict,
  SHELTER_NODE_SIZE
};

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
      r_arr_push_back(p_cached_node->p_parents_list, arrow);
      r_arr_push_back(p_parent_node->p_children_list, arrow);
      FREE(1);
    }

    return R_SEXP_ITERATE_skip;
  }

  struct r_dyn_array* p_node_arr = p_state->p_node_arr;

  // Shelter node objects in the dictionary
  sexp* node_shelter = KEEP(r_new_list(SHELTER_NODE_SIZE));

  sexp* id = KEEP(r_sexp_address(x));
  r_list_poke(node_shelter, SHELTER_NODE_id, id);

  sexp* env = new_node_environment();
  r_list_poke(node_shelter, SHELTER_NODE_env, env);

  // Store node location in the stack so we can update the list of
  // parents when the node is reached again
  sexp* node_location = r_int(p_node_arr->count);
  r_list_poke(node_shelter, SHELTER_NODE_location, node_location);

  struct r_dyn_array* p_parents_list = new_arrow_list(r_null);
  r_list_poke(node_shelter, SHELTER_NODE_parents_dict, p_parents_list->shelter);

  struct r_dyn_array* p_children_list = new_arrow_list(x);
  r_list_poke(node_shelter, SHELTER_NODE_children_dict, p_children_list->shelter);
  
  // Only NULL when parent is root node
  if (p_parent_node) {
    sexp* arrow = new_arrow(parent_node_env, env, depth, rel, i);
    r_arr_push_back(p_parents_list, arrow);
    r_arr_push_back(p_parent_node->p_children_list, arrow);
  }

  struct node node = {
    .env = env,
    .id = id,
    .type = type,
    .self_size = sexp_self_size(x, type),
    .p_parents_list = p_parents_list,
    .p_children_list = p_children_list
  };
  r_arr_push_back(p_state->p_node_arr, &node);

  r_dict_put(p_state->p_dict, x, node_shelter);
  FREE(2);


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

// Nodes ------------------------------------------------------------------

static sexp* node_template_env = NULL;

static
sexp* new_node_environment() {
  sexp* env = KEEP(Rf_allocSExp(ENVSXP));

  SET_ENCLOS(env, ENCLOS(node_template_env));
  SET_HASHTAB(env, r_copy(HASHTAB(node_template_env)));
  SET_FRAME(env, r_copy(FRAME(node_template_env)));
  r_poke_attrib(env, r_attrib(node_template_env));
  r_mark_object(env);

  FREE(1);
  return env;
}

static
bool is_memtools_node(sexp* node) {
  return
    r_typeof(node) == r_type_environment &&
    r_inherits(node, "memtools_node");
}


// Arrows -----------------------------------------------------------------

static
struct r_dyn_array* new_arrow_list(sexp* x) {
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

  return r_new_dyn_vector(r_type_list, n);
}

enum arrow_locs {
  ARROW_LOCS_parent = 0,
  ARROW_LOCS_child,
  ARROW_LOCS_depth,
  ARROW_LOCS_rel,
  ARROW_LOCS_i,
  ARROW_LOCS_name,
  ARROW_SIZE
};
static
const char* v_arrow_names_c_strs[ARROW_SIZE] = {
  "parent",
  "child",
  "depth",
  "rel",
  "i",
  "name"
};
static
sexp* arrow_names = NULL;

static
sexp* new_arrow(sexp* parent_node,
                sexp* child_node,
                int depth,
                enum r_node_relation rel,
                r_ssize i) {
  sexp* arrow = KEEP(r_new_vector(r_type_list, ARROW_SIZE));

  r_list_poke(arrow, ARROW_LOCS_parent, parent_node);
  r_list_poke(arrow, ARROW_LOCS_child, child_node);
  r_list_poke(arrow, ARROW_LOCS_depth, r_int(depth));
  r_list_poke(arrow, ARROW_LOCS_rel, r_chr(r_node_relation_as_c_string(rel)));
  r_list_poke(arrow, ARROW_LOCS_i, r_len(i));
  r_list_poke(arrow, ARROW_LOCS_name, r_null); // TODO: Fetch name

  r_attrib_poke_names(arrow, arrow_names);

  FREE(1);
  return arrow;
}


// Node size --------------------------------------------------------------

static
r_ssize node_size(sexp* node) {
  if (!is_memtools_node(node)) {
    r_abort("`node` must be a memtools node.");
  }

  struct r_dict* p_seen = r_new_dict(NODES_INIT_SIZE);
  KEEP(p_seen->shelter);

  struct r_dyn_array* p_stack = r_new_dyn_array(sizeof(sexp*), NODES_INIT_SIZE);
  KEEP(p_stack->shelter);

  r_ssize size = 0;
  r_arr_push_back(p_stack, &node);

  while (p_stack->count) {
    sexp* node = *((sexp**) r_arr_pop_back(p_stack));

    sexp* node_id = KEEP(r_env_find(node, syms.id));
    bool seen = !r_dict_put(p_seen, r_chr_get(node_id, 0), r_null);
    FREE(1);
    if (seen) {
      continue;
    }

    sexp* node_size = KEEP(r_env_find(node, syms.self_size));
    size += r_as_ssize(node_size);
    FREE(1);

    sexp* children = KEEP(r_env_find(node, syms.children));
    r_ssize n_children = r_length(children);
    sexp* const * v_children = r_list_deref_const(children);

    for (r_ssize i = 0; i < n_children; ++i) {
      sexp* child = r_list_get(v_children[i], ARROW_LOCS_child);
      r_arr_push_back(p_stack, &child);
    }

    FREE(1);
  }

  FREE(2);
  return size;
}

sexp* c_ptr_node_size(sexp* node) {
  return r_len(node_size(node));
}


void init_snapshot() {
  size_t df_names_size = R_ARR_SIZEOF(snapshot_df_names_c_strings);
  size_t df_types_size = R_ARR_SIZEOF(snapshot_df_types);
  RLANG_ASSERT(df_names_size == df_types_size);

  snapshot_df_names = r_chr_n(snapshot_df_names_c_strings, df_names_size);
  r_preserve_global(snapshot_df_names);

  arrow_names = r_chr_n(v_arrow_names_c_strs, ARROW_SIZE);
  r_preserve_global(arrow_names);

  syms.id = r_sym("id");
  syms.type = r_sym("type");
  syms.self_size = r_sym("self_size");
  syms.parents = r_sym("parents");
  syms.children = r_sym("children");

  node_template_env = r_preserve_global(r_new_environment(r_empty_env, 5));
  r_attrib_poke_class(node_template_env, r_chr("memtools_node"));
  r_mark_shared(r_attrib(node_template_env));

  r_env_poke(node_template_env, syms.id, r_null);
  r_env_poke(node_template_env, syms.type, r_null);
  r_env_poke(node_template_env, syms.self_size, r_null);
  r_env_poke(node_template_env, syms.parents, r_null);
  r_env_poke(node_template_env, syms.children, r_null);
}
