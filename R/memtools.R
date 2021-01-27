#' @useDynLib memtools, .registration = TRUE
#' @import rlang
NULL

mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

addr_deref <- function(x) {
  .Call(c_ptr_addr_deref, x)
}

sexp_self_size <- function(x) {
  .Call(c_ptr_sexp_self_size, x)
}

size_node <- function(x) {
  as.integer(utils::object.size(new_node(NULL)))
}
size_vector <- function(x) {
  as.integer(utils::object.size(logical()))
}
