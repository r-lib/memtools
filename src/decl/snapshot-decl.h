static
sexp* arrow_list_compact(sexp* x);

static
struct snapshot_data_vector* new_data_vector(size_t size);

static
void data_vec_grow(struct snapshot_data_vector* p_vec, r_ssize i);

static
void data_vec_push(struct snapshot_data_vector* p_vec);

static
struct snapshot_node_vector* new_node_vector(size_t size);

static
void node_vec_grow(struct snapshot_node_vector* p_vec, r_ssize i);

static
void node_vec_push(struct snapshot_node_vector* p_vec,
                   struct snapshot_node node);

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
