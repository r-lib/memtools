#ifndef MEMTOOLS_ARROW_H
#define MEMTOOLS_ARROW_H


#define ARROWS_INIT_SIZE 2
#define ARROWS_GROWTH_FACTOR 2

enum arrow_locs {
  ARROW_LOCS_from = 0,
  ARROW_LOCS_to,
  ARROW_LOCS_depth,
  ARROW_LOCS_rel,
  ARROW_LOCS_i,
  ARROW_LOCS_name,
  ARROW_SIZE
};

sexp* new_arrow(sexp* parent_node,
                sexp* child_node,
                int depth,
                enum r_node_relation rel,
                r_ssize i);

struct r_dyn_array* new_arrow_list(sexp* x);


#endif
