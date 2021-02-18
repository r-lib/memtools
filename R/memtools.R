#' @useDynLib memtools, .registration = TRUE
#' @import rlang
NULL

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

node_dominators <- function(parents) {
  doms <- .Call(ffi_node_dominators, parents)
  doms <- doms + 1L
  doms[!doms] <- NA

  tibble::tibble(
    node = names(parents),
    dom = names(parents)[doms],
    dom_i = doms
  )
}
