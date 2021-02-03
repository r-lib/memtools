static
sexp* arrow_list_compact(sexp* x);

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
sexp* new_arrow_list(sexp* x);

static
sexp* new_arrow(sexp* id,
                int depth,
                sexp* parent,
                enum r_node_relation rel,
                r_ssize i);

static
void node_push_arrow(struct snapshot_node* node,
                     sexp* arrow,
                     sexp* shelter);
