
static struct snapshot_data new_snapshot_data();
static void check_stack(struct snapshot_data* data, int depth);
static sexp* new_snapshot_node(sexp* id, enum r_type type);
static r_ssize obj_size(sexp* x);
