#' @useDynLib memtools, .registration = TRUE
#' @import rlang
NULL

#' Dereference an object address
#' @param x A string containing a valid memory address of an R object
#'   in hexadecimal format. Can also be a node whose `id` field
#'   will be dereferenced.
#' @return The object at the address pointed to by `x`.
#' @export
deref <- function(x) {
  if (is_memtools_node(x)) {
    return(addr_deref(x$id))
  }
  if (is_string(x)) {
    return(addr_deref(x))
  }

  abort("`x` must be a memtools node or a string.")
}
addr_deref <- function(x) {
  .Call(c_ptr_addr_deref, x)
}

sexp_self_size <- function(x) {
  .Call(c_ptr_sexp_self_size, x)
}

#' @export
rlang::obj_address

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

  # Set dominator of root node to `NA`
  idom[[1]] <- NA

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

info <- function() {
  i <- if (is_installed("cli")) cli::symbol$info else "i"
  cyan(i)
}
has_crayon <- function() is_installed("crayon") && crayon::has_color()
cyan <- function(x) if (has_crayon()) crayon::cyan(x) else x
