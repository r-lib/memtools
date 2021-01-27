#' @useDynLib memtools, .registration = TRUE
NULL

mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

sexp_deref <- function(x) {
  .Call(c_ptr_sexp_deref, x)
}
