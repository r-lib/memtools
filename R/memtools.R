#' @useDynLib memtools, .registration = TRUE
#' @import rlang
NULL

mem_node_size <- function(x) {
  .Call(c_ptr_mem_node_size, x)
}

#' @export
mem_node_dominated_ids <- function(x) {
  stopifnot(inherits(x, "memtools_node"))

  ids <- chr()

  for (node in x$dominated) {
    ids <- c(ids, node$id, mem_node_dominated_ids(node))
  }

  ids
}


#' @export
print.memtools_node <- function(x, ...) {
  writeLines(sprintf("<memtools/node>"))
  writeLines(sprintf("id: \"%s\"", x$id))
  writeLines(sprintf("type: \"%s\"", x$type))
  writeLines(sprintf("self_size: %d", x$self_size))
  writeLines(sprintf("parents: %s", pillar::obj_sum(list(x$parents))))
  writeLines(sprintf("children: %s", pillar::obj_sum(list(x$children))))
  writeLines(sprintf("dominator: <%s>", pillar::obj_sum(x$dominator)))
  writeLines(sprintf("dominated: %s", pillar::obj_sum(list(x$dominated))))
}
#' @export
print.memtools_arrow <- function(x, ...) {
  writeLines(sprintf("<memtools/arrow>"))
  writeLines(sprintf("from: <%s>", pillar::obj_sum(x$from)))
  writeLines(sprintf("to: <%s>", pillar::obj_sum(x$to)))
  writeLines(sprintf("depth: %d", x$depth))
  writeLines(sprintf("rel: \"%s\"", x$rel))
  writeLines(sprintf("i: %d", x$i))
  writeLines(sprintf("name: \"%s\"", x$name))
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
  res <- .Call(ffi_node_dominators, parents)
  idom <- res[[1]]
  sdom <- res[[2]]

  tibble0(
    node = names(parents),
    idom = names(parents)[idom],
    sdom = names(parents)[sdom],
    idom_i = idom,
    sdom_i = sdom
  )
}

tibble0 <- function(...) {
  df <- vctrs::data_frame(...)
  tibble::new_tibble(df, nrow = nrow(df))
}
