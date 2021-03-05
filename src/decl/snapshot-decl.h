static
struct snapshot_state* new_snapshot_state();

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

static
sexp* new_snapshot_df(struct snapshot_state* p_state);

static inline
bool is_mem_stash(sexp* x);
