#' @useDynLib memtools, .registration = TRUE
NULL

mem_snapshot <- function(x) {
  .Call(ptr_snapshot, x)
}
