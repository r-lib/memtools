static
size_t snapshot_stack_size(size_t n);

static
size_t snapshot_nodes_size(size_t n);

static inline
void stack_grow(struct snapshot_stack* x, r_ssize i);

static inline
void nodes_grow(struct snapshot_nodes* x, r_ssize i);

static
void nodes_push(struct snapshot_nodes* nodes, struct snapshot_node node);

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
