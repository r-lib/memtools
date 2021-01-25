#' @useDynLib memtools, .registration = TRUE
NULL

mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}
