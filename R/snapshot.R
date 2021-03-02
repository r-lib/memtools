#' Create a memory snapshot
#' @param x An R object. This becomes the root of the graph of R
#'   objects captured in the snapshot.
#' @return A data frame containing nodes, arrows between these nodes,
#'   and metadata.
#' @seealso [mem_stash()] to prevent objects from being recorded in a
#'   snapshot.
#' @export
mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

#' Stash objects
#'
#' Objects contained in a stash are never recorded by
#' [mem_snapshot()].
#'
#' @param ... Objects to stash.
#' @return An environment containing the stashed objects. You can add
#'   objects to this environment in the usual fashion.
#' @export
mem_stash <- function(...) {
  .Call(ffi_new_stash, env(empty_env(), ...))
}

#' Roots of interest
#' @description
#' * `root_cpp11()` returns the precious list of cpp11. This is a
#'   doubly linked list preserved in R's own precious list.
#' @export
root_cpp11 <- function() {
  .Call(ffi_root_cpp11)
}
