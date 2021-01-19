
static
void grow_stack(struct snapshot_data* data, int depth);

static
struct snapshot_data* new_snapshot_data();

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
sexp* new_snapshot_node(sexp* id, enum r_type type);

static
r_ssize obj_size(sexp* x);
