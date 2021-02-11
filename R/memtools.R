#' @useDynLib memtools, .registration = TRUE
#' @import rlang
NULL

mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

mem_node_size <- function(x) {
  .Call(c_ptr_mem_node_size, x)
}

#' @export
print.memtools_node <- function(x, ...) {
  writeLines(sprintf("<memtools/node>"))
  writeLines(sprintf("id: \"%s\"", x$id))
  writeLines(sprintf("type: \"%s\"", x$type))
  writeLines(sprintf("self_size: %d", x$self_size))
  writeLines(sprintf("parents: %s", pillar::obj_sum(list(x$parents))))
  writeLines(sprintf("children: %s", pillar::obj_sum(list(x$children))))
}

addr_deref <- function(x) {
  .Call(c_ptr_addr_deref, x)
}

sexp_self_size <- function(x) {
  .Call(c_ptr_sexp_self_size, x)
}

#' @export
rlang::sexp_address

size_node <- function(x) {
  as.integer(utils::object.size(new_node(NULL)))
}
size_vector <- function(x) {
  as.integer(utils::object.size(logical()))
}
