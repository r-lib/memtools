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
struct snapshot_node* get_cached_node(struct snapshot_state* p_state,
                                      sexp* x);
static
struct snapshot_node* get_cached_parent_node(struct snapshot_state* p_state,
                                             sexp* parent);

static
struct r_dict* new_arrow_dict(sexp* x);

static
sexp* new_arrow(sexp* parent_node,
                sexp* child_node,
                int depth,
                enum r_node_relation rel,
                r_ssize i);

static
sexp* new_node_environment();
