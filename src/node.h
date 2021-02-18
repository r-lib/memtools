#ifndef MEMTOOLS_NODE_H
#define MEMTOOLS_NODE_H


#define NODES_INIT_SIZE 65536
#define NODES_GROWTH_FACTOR 2

struct node {
  sexp* shelter;

  sexp* env;
  sexp* id;
  enum r_type type;
  r_ssize self_size;
  struct r_dyn_array* p_parents_list;
  struct r_dyn_array* p_children_list;

  int depth_first_loc;
};

enum shelter_node {
  SHELTER_NODE_location = 0,
  SHELTER_NODE_id,
  SHELTER_NODE_env,
  SHELTER_NODE_parents_dict,
  SHELTER_NODE_parents_locs,
  SHELTER_NODE_children_dict,
  SHELTER_NODE_SIZE
};


void init_node(struct node* p_node,
               sexp* x,
               enum r_type type,
               int depth_first_loc);

bool is_node_shelter(sexp* x);


#endif
