#ifndef MEMTOOLS_DOMINANCE_H
#define MEMTOOLS_DOMINANCE_H

struct dom_info {
  int idom;
  int sdom;
};

sexp* node_dominators(struct r_pair_ptr_ssize* vv_parents,
                      int n_nodes,
                      struct dom_info** out_v_dom);


#endif
