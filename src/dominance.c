#include <rlang.h>

#include "dominance.h"
#include "node.h"

#include "decl/dominance-decl.h"


/* Fixed arrays of integers:
 * - buckets
 * - parent
 * - idom
 * - sdom
 * - dsu_ancestor // Ancestor with least sdom
 * - 
 */

struct dsu_info {
  int ancestor;
  int best;
};

int dsu_find_lowest_sdom(struct dsu_info* v_dsu,
                         struct dom_info* v_dom,
                         int v) {
  return -1;
}

void dsu_union(struct dsu_info* v_dsu, int v, int w) {
  // TODO: Balance forest
  v_dsu[w].ancestor = v;
}

sexp* node_dominators(struct r_pair_ptr_ssize* vv_parents,
                      int n_nodes,
                      struct dom_info** out_v_dom) {
  sexp* dom = KEEP(r_new_raw(sizeof(struct dom_info) * n_nodes));
  struct dom_info* v_dom = r_raw_deref(dom);

  sexp* dsu = KEEP(r_new_raw(sizeof(struct dsu_info) * n_nodes));
  struct dsu_info* v_dsu = r_raw_deref(dsu);

  struct dom_info dom_init = { .idom = -1, .sdom = -1 };
  R_MEM_SET(struct dom_info, v_dom, dom_init, n_nodes);

  struct dsu_info dsu_init = { .ancestor = -1, .best = -1 };
  R_MEM_SET(struct dsu_info, v_dsu, dsu_init, n_nodes);

  sexp* buckets = KEEP(r_new_integer(n_nodes));
  r_int_fill_iota(buckets);
  int* v_buckets = r_int_deref(buckets);

  for (int i = n_nodes - 1; i > 0; --i) {
    if (i % 100) {
      r_yield_interrupt();
    }

    int* v_parents = vv_parents[i].ptr;
    int n_parents = vv_parents[i].size;
    int parent = v_parents[0];

    // Compute trivial dominators
    for (int v = v_buckets[i]; v != i; v = v_buckets[v]) {
      int u = dsu_find_lowest_sdom(v_dsu, v_dom, v);
      if (v_dom[u].sdom < i) {
        v_dom[v].idom = u;
      } else {
        v_dom[v].idom = i;
      }
    }

    // Compute semi-dominators
    v_dom[i].sdom = parent;
    for (int j = 1; j < n_parents; ++j) {
      int u = dsu_find_lowest_sdom(v_dsu, v_dom, v_parents[j]);
      if (v_dom[u].sdom < v_dom[i].sdom) {
        v_dom[i].sdom = v_dom[u].sdom;
      }
    }

    dsu_union(v_dsu, parent, i);

    {
      int sdom = v_dom[i].sdom;
      if (parent == sdom) {
        v_dom[i].idom = parent;
      } else {
        v_buckets[i] = v_buckets[sdom];
        v_buckets[sdom] = i;
      }
    }
  }

  // Finish dominators
  for (int v = v_buckets[0]; v != -1; v = v_buckets[v]) {
    v_dom[v].idom = -1;
  }

  for (int i = 0; i < n_nodes; ++i) {
    Rprintf("idom for node %d: %d\n", i, v_dom[i].idom);
  }

  FREE(3);
  *out_v_dom = v_dom;
  return dom;
}


// [[ register() ]]
sexp* ffi_node_dominators(sexp* parents) {
  if (r_typeof(parents) != r_type_list) {
    r_abort("`parents` must be a list.");
  }
  r_ssize n = r_length(parents);

  struct r_pair_ptr_ssize* v_parents;
  KEEP(r_list_of_as_ptr_ssize(parents, r_type_integer, &v_parents));

  struct dom_info* v_dom;
  KEEP(node_dominators(v_parents, n, &v_dom));

  sexp* out = r_new_integer(n);
  int* v_out = r_int_deref(out);
  for (r_ssize i = 0; i < n; ++i) {
    v_out[i] = v_dom[i].idom;
  }

  FREE(2);
  return out;
}
