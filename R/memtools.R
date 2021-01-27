#' @useDynLib memtools, .registration = TRUE
NULL

mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

addr_deref <- function(x) {
  .Call(c_ptr_addr_deref, x)
}
