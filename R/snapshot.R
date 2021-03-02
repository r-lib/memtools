#' Create a memory snapshot
#' @param x An R object. This becomes the root of the graph of R
#'   objects captured in the snapshot.
#' @return A data frame containing nodes, arrows between these nodes,
#'   and metadata.
#' @export
mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

#' @export
mem_stash <- function(...) {
  .Call(ffi_new_stash, env(empty_env(), ...))
}
