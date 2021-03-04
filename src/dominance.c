#include <rlang.h>

#include "dominance.h"
#include "node.h"

#include "decl/dominance-decl.h"

/**
 * Semi-NCA algorithm for computing dominance tree.
 *
 * References:
 * - Thomas Lengauer and Robert E. Tarjan, 1979, ACM Transactions on
 *   Programming Languages and Systems 1(1)
 * - Loukas Georgiadis and Robert E. Tarjan and Renato F. Werneck,
 *   2006, Finding Dominators in Practice, Journal of Graph Algorithms
 *   and Applications 10(1)
 * 
 * See also
 * <https://github.com/JuliaLang/julia/blob/master/base/compiler/ssair/domtree.jl>
 * for a similar implementation
 */



// p.77: "The core of the computation is performed by a _link-eval_
// data structure [which] maintains a forest `F` that is a subgraph of
// `T`. subject to the following operations: `link(v, x)` and
// `eval(v)`.". See also
// <https://en.wikipedia.org/wiki/Disjoint-set_data_structure>. The
// forest maintains trees whose roots are the semi-dominators.
struct forest_info {
  int ancestor;
  int label;
};

// Get root node for `i` in the forest `F`. Initially returns
// `v`. Once `v` has been added to the forest, returns `sdom(v)`.
static
int forest_eval_lowest_sdom(struct forest_info* v_forest,
                            struct dom_info* v_dom,
                            int x,
                            int n) {
  int x_ancestor = v_forest[x].ancestor;
  if (x_ancestor < 0) {
    return n ? -1 : x;
  }

  int y = forest_eval_lowest_sdom(v_forest,
                                  v_dom,
                                  x_ancestor,
                                  n + 1);
  if (y < 0) {
    return x;
  }

  int x_new_label = v_forest[v_forest[x].ancestor].label;
  int x_old_label = v_forest[x].label;
  if (v_dom[x_new_label].sdom < v_dom[x_old_label].sdom) {
    v_forest[x].label = x_new_label;
  }

  v_forest[x].ancestor = y;

  return n ? y : v_forest[x].label;
}

static
void forest_link(struct forest_info* v_forest, int parent, int i) {
  v_forest[i].ancestor = parent;
}

sexp* node_dominators(struct r_pair_ptr_ssize* vv_parents,
                      int n_nodes) {
  struct dom_info* v_dom;
  KEEP(node_dominators0(vv_parents, n_nodes, &v_dom));

  sexp* idom = r_alloc_integer(n_nodes);
  int* v_idom = r_int_deref(idom);

  for (int i = 0; i < n_nodes; ++i) {
    v_idom[i] = v_dom[i].idom + 1;
  }

  FREE(1);
  return idom;
}

sexp* node_dominators0(struct r_pair_ptr_ssize* vv_parents,
                       int n_nodes,
                       struct dom_info** out_v_dom) {
  sexp* dom = KEEP(r_alloc_raw(sizeof(struct dom_info) * n_nodes));
  struct dom_info* v_dom = r_raw_deref(dom);

  sexp* forest = KEEP(r_alloc_raw(sizeof(struct forest_info) * n_nodes));
  struct forest_info* v_forest = r_raw_deref(forest);

  for (int i = 0; i < n_nodes; ++i) {
    v_dom[i] = (struct dom_info) { .idom = i, .sdom = i };
  }

  struct forest_info forest_init = { .ancestor = -1, .label = -1 };
  R_MEM_SET(struct forest_info, v_forest, forest_init, n_nodes);

  // Compute semi-dominators
  for (int i = n_nodes - 1; i > 0; --i) {
    if (i % 1000) {
      r_yield_interrupt();
    }

    int* v_parents = vv_parents[i].ptr;
    int n_parents = vv_parents[i].size;
    int parent = v_parents[0];

    v_dom[i].idom = parent;
    v_dom[i].sdom = parent;
    for (int j = 0; j < n_parents; ++j) {
      int u = forest_eval_lowest_sdom(v_forest, v_dom, v_parents[j], 0);
      int u_sdom = v_dom[u].sdom;
      if (u_sdom < v_dom[i].sdom) {
        v_dom[i].sdom = u_sdom;
      }
    }

    // Maintain a forest of the processed vertices
    forest_link(v_forest, parent, i);
  }

  // Perform NCA step
  v_dom[0].idom = -1;
  for (int i = 1; i < n_nodes; ++i) {
    int idom = v_dom[i].idom;
    int sdom = v_dom[i].sdom;
    while (idom > sdom) {
      idom = v_dom[idom].idom;
    }
    v_dom[i].idom = idom;
  }

  FREE(2);
  *out_v_dom = v_dom;
  return dom;
}


// [[ register() ]]
sexp* ffi_node_dominators(sexp* parents) {
  if (r_typeof(parents) != R_TYPE_list) {
    r_abort("`parents` must be a list.");
  }
  r_ssize n = r_length(parents);

  struct r_pair_ptr_ssize* v_parents;
  KEEP(r_list_of_as_ptr_ssize(parents, R_TYPE_integer, &v_parents));

  struct dom_info* v_dom;
  KEEP(node_dominators0(v_parents, n, &v_dom));

  sexp* out = KEEP(r_alloc_list(2));

  sexp* dom = r_alloc_integer(n);
  r_list_poke(out, 0, dom);

  sexp* sdom = r_alloc_integer(n);
  r_list_poke(out, 1, sdom);

  int* v_dom_out = r_int_deref(dom);
  int* v_sdom_out = r_int_deref(sdom);

  for (r_ssize i = 0; i < n; ++i) {
    v_dom_out[i] = v_dom[i].idom + 1;
    v_sdom_out[i] = v_dom[i].sdom + 1;
  }

  FREE(3);
  return out;
}
