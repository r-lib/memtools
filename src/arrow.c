#include <rlang.h>
#include "arrow.h"


static
sexp* arrow_names = NULL;
static
sexp* arrow_class = NULL;

static
const char* v_arrow_names_c_strs[ARROW_SIZE] = {
  "from",
  "to",
  "depth",
  "rel",
  "i",
  "name"
};


sexp* new_arrow(sexp* parent_node,
                sexp* child_node,
                int depth,
                enum r_node_relation rel,
                r_ssize i) {
  sexp* arrow = KEEP(r_alloc_list(ARROW_SIZE));

  r_list_poke(arrow, ARROW_LOCS_from, parent_node);
  r_list_poke(arrow, ARROW_LOCS_to, child_node);
  r_list_poke(arrow, ARROW_LOCS_depth, r_int(depth));
  r_list_poke(arrow, ARROW_LOCS_rel, r_chr(r_node_relation_as_c_string(rel)));
  r_list_poke(arrow, ARROW_LOCS_i, r_len(i));
  r_list_poke(arrow, ARROW_LOCS_name, r_null); // TODO: Fetch name

  r_attrib_poke_names(arrow, arrow_names);
  r_attrib_poke_class(arrow, arrow_class);

  FREE(1);
  return arrow;
}


struct r_dyn_array* new_arrow_list(sexp* x) {
  // Make space for a few arrows per node. The arrow lists are
  // compacted later on.
  r_ssize n = ARROWS_INIT_SIZE;

  switch (r_typeof(x)) {
  default:
    break;
  case R_TYPE_character:
  case R_TYPE_expression:
  case R_TYPE_list:
    n += r_length(x);
    break;
  }

  return r_new_dyn_vector(R_TYPE_list, n);
}


void init_library_arrow() {
  arrow_names = r_chr_n(v_arrow_names_c_strs, ARROW_SIZE);
  r_preserve_global(arrow_names);

  arrow_class = r_chr("memtools_arrow");
  r_preserve_global(arrow_class);
}
