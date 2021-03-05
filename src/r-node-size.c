#include <rlang.h>
#include "arrow.h"
#include "globals.h"
#include "node.h"

static
r_ssize node_size(sexp* node) {
  if (!is_node_shelter(node)) {
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
      sexp* child = r_list_get(v_children[i], ARROW_LOCS_to);
      r_arr_push_back(p_stack, &child);
    }

    FREE(1);
  }

  FREE(2);
  return size;
}

// [[ register() ]]
sexp* c_ptr_node_size(sexp* node) {
  return r_len(node_size(node));
}
