#' @useDynLib memtools, .registration = TRUE
#' @import rlang
NULL

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
