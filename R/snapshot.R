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
arg_as_mem_node <- function(x, snapshot, arg = substitute(x)) {
  if (is_integerish(x, n = 1, finite = TRUE)) {
    snapshot$node[[x]]
  } else if (is_string(x)) {
    i <- which(x == snapshot$id)
    if (length(i) != 1) {
      abort("Can't find node in snapshot.")
    }
    snapshot$node[[i]]
  } else {
    arg_check_mem_node(x, arg = arg)
    x
  }
}


#' Add and retrieve igraph structure of snapshot
#'
#' `mem_igraph()` retrieves the igraph structure of `snapshot`. The
#' igraph structure is cached inside `snapshot` by side effect to
#' avoid recomputing it multiple times.
#'
#' The igraph is ordered according to the original data frame
#' ordering. Rearranging the rows of `snapshot` does not rearrange the
#' igraph.
#'
#' @param snapshot A memtools snapshot.
#' @export
#' @examples
#' s <- mem_snapshot(list(1, list(2)))
#'
#' # Retrieve the igraph. It is first computed and then cached inside
#' # the snapshot so it does not need to be recreated again.
#' g <- mem_igraph(s)
#'
#' # Work with igraph
#' igraph::gsize(g)
mem_igraph <- function(snapshot) {
  mem_poke_igraph(snapshot)
  attr(snapshot, "mem_igraph")$igraph
}
mem_poke_igraph <- function(snapshot) {
  arg_check_snapshot(snapshot)

  igraph_env <- attr(snapshot, "mem_igraph")
  if (!is_null(igraph_env$igraph)) {
    return(snapshot)
  }

  adj_list <- attr(snapshot, "mem_adj_list")
  adj_list <- lapply(adj_list, `+`, 1L)

  if (!is_false(peek_option("memtools_verbose"))) {
    writeLines("Adding igraph structure to snapshot as `mem_igraph` attribute...")
  }
  igraph_env$igraph <- igraph::graph_from_adj_list(adj_list)

  invisible(NULL)
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

#' Find all shortest paths
#'
#' Call [mem_set_igraph()] on `snapshot`
#' @param snapshot A memory snapshot data frame created by
#'   [mem_snapshot()].
#' @param to,from Nodes from the `node` column of `snapshot`.  If
#'   `from` is not supplied, the paths from the root are taken by
#'   default. Can also be indices into `snapshot$node`.
mem_shortest_paths <- function(snapshot, to, from = NULL) {
  check_installed("igraph")
  arg_check_snapshot(snapshot)

  if (is_null(from)) {
    from <- snapshot$node[[1]]
  }
  to <- arg_as_mem_node(to, snapshot)
  from <- arg_as_mem_node(from, snapshot)

  i <- match(c(from$id, to$id), snapshot$id)
  if (anyNA(i)) {
    abort("Can't find nodes in snapshot.")
  }
  if (length(i) != 2) {
    abort("Unexpected duplicate nodes in snapshot.")
  }

  graph <- mem_igraph(snapshot)
  paths <- igraph::all_shortest_paths(graph, i[[1]], i[[2]], mode = "in")

  lapply(paths$res, lapply, function(x) snapshot$node[[x]])
}
