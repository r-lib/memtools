#include <rlang.h>
#include "arrow.h"
#include "memtools.h"
#include "node.h"
#include "size.h"

#include "decl/node-decl.h"


void init_node(struct node* p_node,
               sexp* x,
               enum r_type type,
               r_ssize loc) {
  // Shelter node objects in the dictionary
  sexp* shelter = KEEP(r_new_list(SHELTER_NODE_SIZE));

  sexp* id = r_sexp_address(x);
  r_list_poke(shelter, SHELTER_NODE_id, id);

  sexp* env = new_node_environment();
  r_list_poke(shelter, SHELTER_NODE_env, env);

  // Store node location in the stack so we can update the list of
  // parents when the node is reached again
  sexp* node_location = r_int(loc);
  r_list_poke(shelter, SHELTER_NODE_location, node_location);

  struct r_dyn_array* p_parents_list = new_arrow_list(r_null);
  r_list_poke(shelter, SHELTER_NODE_parents_dict, p_parents_list->shelter);

  struct r_dyn_array* p_children_list = new_arrow_list(x);
  r_list_poke(shelter, SHELTER_NODE_children_dict, p_children_list->shelter);

  struct r_dyn_array* p_parents_locs = r_new_dyn_vector(r_type_integer, ARROWS_INIT_SIZE);
  r_list_poke(shelter, SHELTER_NODE_parents_locs, p_parents_locs->shelter);

  *p_node = (struct node) {
    .shelter = shelter,
    .env = env,
    .id = id,
    .type = type,
    .self_size = sexp_self_size(x, type),
    .p_parents_list = p_parents_list,
    .p_children_list = p_children_list,

    .loc = loc,
    .p_parents_locs = p_parents_locs,
  };

  FREE(1);
}


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

bool is_node_shelter(sexp* x) {
  return
    r_typeof(x) == r_type_environment &&
    r_inherits(x, "memtools_node");
}


void init_library_node() {
  node_template_env = r_preserve_global(r_new_environment(r_empty_env, 5));
  r_attrib_poke_class(node_template_env, r_chr("memtools_node"));
  r_mark_shared(r_attrib(node_template_env));

  r_env_poke(node_template_env, syms.id, r_null);
  r_env_poke(node_template_env, syms.type, r_null);
  r_env_poke(node_template_env, syms.self_size, r_null);
  r_env_poke(node_template_env, syms.parents, r_null);
  r_env_poke(node_template_env, syms.children, r_null);
}
