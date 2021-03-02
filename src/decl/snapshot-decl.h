static
struct snapshot_state* new_snapshot_state();

static
enum r_sexp_iterate snapshot_iterator(void* data,
                                      sexp* x,
                                      enum r_type type,
                                      int depth,
                                      sexp* parent,
                                      enum r_node_relation rel,
                                      r_ssize i,
                                      enum r_node_direction dir);

static
struct node* get_cached_node(struct snapshot_state* p_state,
                             sexp* x);
static
struct node* get_cached_parent_node(struct snapshot_state* p_state,
                                    sexp* parent);
static
sexp* dominance_info(const struct r_pair_ptr_ssize* vv_dominated,
                     const struct node* v_nodes,
                     int n_nodes,
                     struct dom_tree_info** out_v_info);

static
void dominance_info_rec(int i,
                        int parent,
                        int depth,
                        const struct r_pair_ptr_ssize* vv_dominated,
                        const struct node* v_nodes,
                        struct dom_tree_info* v_info);
