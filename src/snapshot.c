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

static
const char* snapshot_df_names_c_strings[] = {
  "id",
  "type",
  "node",
  "parents",
};
static
const enum r_type snapshot_df_types[] = {
  r_type_character,
  r_type_character,
  r_type_list,
  r_type_list,
};
enum snapshot_df_locs {
  SNAPSHOT_DF_LOCS_id = 0,
  SNAPSHOT_DF_LOCS_type,
  SNAPSHOT_DF_LOCS_node,
  SNAPSHOT_DF_LOCS_parents,
};

#define SNAPSHOT_DF_SIZE R_ARR_SIZEOF(snapshot_df_types)

static
sexp* snapshot_df_names = NULL;

struct snapshot_node {
  sexp* env;
  sexp* id;
  enum r_type type;
  r_ssize self_size;
  struct r_dict* p_arrow_dict;
};
enum shelter_node {
  SHELTER_NODE_location = 0,
  SHELTER_NODE_env,
  SHELTER_NODE_arrow_dict,
  SHELTER_NODE_max
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

  struct snapshot_node* v_nodes = r_arr_ptr_front(p_node_arr);

  for (r_ssize i = 0; i < n_rows; ++i) {
    struct snapshot_node node = v_nodes[i];

    sexp* node_type_str = KEEP(r_type_as_string(node.type));
    sexp* parents_list = KEEP(r_dict_as_list(node.p_arrow_dict));

    sexp* node_env = node.env;
    r_env_poke(node_env, syms.id, r_str_as_character(node.id));
    r_env_poke(node_env, syms.type, r_str_as_character(node_type_str));
    r_env_poke(node_env, syms.self_size, r_len(node.self_size));
    r_env_poke(node_env, syms.parents, parents_list);

    r_chr_poke(id_col, i, node.id);
    r_chr_poke(type_col, i, node_type_str);
    r_list_poke(node_col, i, node_env);
    r_list_poke(parents_col, i, parents_list);

    FREE(2);
  }

  FREE(2);
  return df;
}

static
struct snapshot_node* get_cached_node(struct r_dyn_array* p_arr, sexp* cached) {
  int i = r_int_get(r_list_get(cached, SHELTER_NODE_location), 0);
  return r_arr_ptr(p_arr, i);
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

  sexp* cached = r_dict_get0(p_state->p_dict, x);

  struct r_dyn_array* p_node_arr = p_state->p_node_arr;

  sexp* id = KEEP(r_sexp_address(x));
  sexp* parent_id = KEEP(r_sexp_address(parent));

  sexp* arrow = r_null;
  if (rel != R_NODE_RELATION_root) {
    arrow = new_arrow(id, depth, parent, rel, i);
  }
  KEEP(arrow);

  if (cached) {
    if (arrow != r_null && dir != R_NODE_DIRECTION_outgoing) {
      struct snapshot_node* p_node = get_cached_node(p_node_arr, cached);
      r_dict_put(p_node->p_arrow_dict, parent_id, arrow);
    }

    FREE(3);
    return R_SEXP_ITERATE_skip;
  }

  // Shelter node objects in the dictionary
  sexp* node_shelter = KEEP(r_new_list(SHELTER_NODE_max));

  // Store node location in the stack so we can update the list of
  // parents when the node is reached again
  sexp* node_location = r_int(p_node_arr->count);
  r_list_poke(node_shelter, SHELTER_NODE_location, node_location);

  struct r_dict* p_arrow_dict = new_arrow_dict(x);
  r_list_poke(node_shelter, SHELTER_NODE_arrow_dict, p_arrow_dict->shelter);

  if (arrow != r_null) {
    r_dict_put(p_arrow_dict, parent_id, arrow);
  }

  sexp* env = new_node_environment();
  r_list_poke(node_shelter, SHELTER_NODE_env, env);

  struct snapshot_node node = {
    .env = env,
    .id = id,
    .type = type,
    .self_size = sexp_self_size(x, type),
    .p_arrow_dict = p_arrow_dict
  };
  r_arr_push_back(p_state->p_node_arr, &node);

  r_dict_put(p_state->p_dict, x, node_shelter);
  FREE(4);


  // Skip bindings of the global environment as they will contain
  // objects from the debugging session, including memory snapshots.
  // TODO: Traverse global env manually to collect hidden symbols
  // starting with a dot.
  if (parent == r_global_env && rel != R_NODE_RELATION_environment_enclos) {
    return R_SEXP_ITERATE_skip;
  }

  return R_SEXP_ITERATE_next;
}


// Snapshot data ----------------------------------------------------------

enum shelter_snapshot {
  SHELTER_SNAPSHOT_state,
  SHELTER_SNAPSHOT_nodes,
  SHELTER_SNAPSHOT_dict,
  SHELTER_SNAPSHOT_max
};

static
struct snapshot_state* new_snapshot_state() {
  sexp* shelter = KEEP(r_new_vector(r_type_list, SHELTER_SNAPSHOT_max));

  sexp* state_shelter = r_new_vector(r_type_raw, sizeof(struct snapshot_state));
  r_list_poke(shelter, SHELTER_SNAPSHOT_state, state_shelter);

  struct r_dyn_array* p_node_arr = r_new_dyn_array(sizeof(struct snapshot_node), NODES_INIT_SIZE);
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


// Arrows -----------------------------------------------------------------

static
struct r_dict* new_arrow_dict(sexp* x) {
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

  return r_new_dict(n);
}

static
const char* v_arrow_names_c_strs[] = {
  "parent",
  "child",
  "depth",
  "rel",
  "i",
  "name"
};
#define ARROW_SIZE R_ARR_SIZEOF(v_arrow_names_c_strs)

enum arrow_locs {
  ARROW_LOCS_parent = 0,
  ARROW_LOCS_child,
  ARROW_LOCS_depth,
  ARROW_LOCS_rel,
  ARROW_LOCS_i,
  ARROW_LOCS_name
};

static
sexp* arrow_names = NULL;

static
sexp* new_arrow(sexp* id,
                int depth,
                sexp* parent,
                enum r_node_relation rel,
                r_ssize i) {
  sexp* arrow = KEEP(r_new_vector(r_type_list, ARROW_SIZE));
  sexp* addr_parent = KEEP(r_sexp_address(parent));

  r_list_poke(arrow, ARROW_LOCS_parent, r_str_as_character(addr_parent));
  r_list_poke(arrow, ARROW_LOCS_child, r_str_as_character(id));
  r_list_poke(arrow, ARROW_LOCS_depth, r_int(depth));
  r_list_poke(arrow, ARROW_LOCS_rel, r_chr(r_node_relation_as_c_string(rel)));
  r_list_poke(arrow, ARROW_LOCS_i, r_len(i));
  r_list_poke(arrow, ARROW_LOCS_name, r_null); // TODO: Fetch name

  r_attrib_poke_names(arrow, arrow_names);

  FREE(2);
  return arrow;
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
