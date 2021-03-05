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

is_snapshot <- function(x) {
  tibble::is_tibble(x) && !is_null(attr(x, "mem_adj_list"))
}
arg_check_snapshot <- function(x, arg = substitute(x)) {
  if (!is_snapshot(x)) {
    msg <- sprintf("`%s` must be a memtools snapshot.", as_string(arg))
    abort(msg)
  }
}

#' Add and retrieve igraph structure of snapshot
#'
#' @description
#' * `mem_set_igraph()` computes the igraph structure of `snapshot`. It
#'   is added to the tibble under the `mem_igraph` attribute.
#'
#' * `mem_igraph()` retrieves the igraph structure of `snapshot`. If
#'   it has already been computed with `mem_set_igraph()` the cached
#'   igraph is returned.
#'
#' The igraph is ordered according to the original data frame
#' ordering. Rearranging the rows of `snapshot` does not rearrange the
#' igraph.
#' @export
#' @param snapshot A memtools snapshot.
#' @examples
#' s <- mem_snapshot(list(1, list(2)))
#'
#' # Compute and store the igraph in the snapshot
#' s <- mem_set_igraph(s)
#'
#' # Retrieve the igraph
#' g <- mem_igraph(s)
#'
#' # Work with igraph
#' igraph::gsize(g)
mem_set_igraph <- function(snapshot) {
  arg_check_snapshot(snapshot)

  igraph <- attr(snapshot, "mem_igraph")
  if (!is_null(igraph)) {
    return(snapshot)
  }

  adj_list <- attr(snapshot, "mem_adj_list")
  adj_list <- lapply(adj_list, `+`, 1L)

  if (!is_false(peek_option("memtools_verbose"))) {
    writeLines("Adding igraph structure to snapshot as `mem_igraph` attribute...")
  }
  g <- igraph::graph_from_adj_list(adj_list)
  attr(snapshot, "mem_igraph") <- g

  snapshot
}
#' @rdname mem_set_igraph
#' @export
mem_igraph <- function(snapshot) {
  snapshot <- mem_set_igraph(snapshot)
  attr(snapshot, "mem_igraph")
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
#'
#' @description
#' These functions return useful starting points to supply to
#' [mem_snapshot()].
#' 
#' * `root_cpp11()` returns the precious list of cpp11. This is a
#'   doubly linked list preserved in R's own precious list.
#'
#' * `root_ns_registry()` returns R's namespace registry as a list. It
#'   contains all the namespaces currently loaded in the R session.
#' @export
root_cpp11 <- function() {
  .Call(ffi_root_cpp11)
}
#' @rdname root_cpp11
#' @export
root_ns_registry <- function() {
  as.list(.Call(ffi_root_ns_registry))
}
