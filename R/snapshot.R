#' Create a memory snapshot
#'
#' Record a memory snapshot for all the R objects reachable from `x`.
#'
#' @param x An R object. This becomes the root of the graph of R
#'   objects captured in the snapshot. A good starting object is the
#'   namespace registry. See [root_ns_registry()].
#' @return A data frame containing nodes, arrows between these nodes,
#'   and metadata.
#' @seealso The GC [roots]. [mem_stash()] to prevent objects from
#'   being recorded in a snapshot. [mem_diff()] to take the difference
#'   between a "before" and "after" snapshot.
#'
#' @section Excluding objects from snapshots:
#' To avoid ulterior snapshots recording previous ones, all objects in
#' the global environment are excluded from the snapshot, unless
#' reachable through another path.
#'
#' If you record a snapshot of the R precious list (see
#' [?roots][roots]), objects of the global environments will be
#' reachable through the global binding cache that lives in the
#' precious list. In that case, exclude these objects from snapshots
#' using [mem_stash()].
#'
#' @details
#' The snapshot keeps all the captured objects alive to make it easy
#' to compare multiple snapshots. The snapshot objects are identified
#' by their address in memory but there is no risk of R reusing a
#' memory address with a different object between runs.
#'
#' @examples
#' # Take a snapshot
#' s <- mem_snapshot(list(1, list(2)))
#' s
#'
#' # Inspect nodes
#' n <- s$node[[1]]
#' n
#'
#' # Inspect arrows
#' n$children
#' @export
mem_snapshot <- function(x) {
  .Call(c_ptr_snapshot, x)
}

mem_id <- function(x) {
  attr(x, "mem_id")
}
mem_node <- function(x) {
  attr(x, "mem_node")
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
    i <- which(x == mem_id(snapshot))
    if (length(i) != 1) {
      abort("Can't find node in snapshot.")
    }
    snapshot$node[[i]]
  } else {
    arg_check_mem_node(x, arg = arg)
    x
  }
}

#' Diff before and after snapshots
#' @param before,after Snapshots created by [mem_snapshot()] before
#'   and after an expression that causes a leak.
#' @return `after` without the rows that exist in `before`. Note that
#'   the nodes in the `node` list-column still refer to nodes in
#'   `before` via `parents`, `children`, etc.
#' @examples
#' e <- new.env(parent = emptyenv())
#'
#' e$x <- 1
#' before <- mem_snapshot(e)
#'
#' e$y <- 2
#' after <- mem_snapshot(e)
#'
#' # The snapshot diff only contains the bucket pairlist node where
#' # `y` is stored, the symbol `y`, and the double value `2` which is
#' # bound to `y`
#' mem_diff(before, after)
#' @export
mem_diff <- function(before, after) {
  new <- !after$id %in% before$id
  after[new, ]
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
  igraph_env$igraph <- igraph::graph_from_adj_list(adj_list, mode = "in")

  invisible(NULL)
}

#' Stash objects
#'
#' Objects contained in a stash are never recorded by
#' [mem_snapshot()]. Note that objects only bound inside the global
#' environments are normally excluded from snapshots. However, if you
#' take a snapshot of the precious list (see [`?roots`][roots]),
#' global objects are reachable through the global cache. In this case
#' you can hide them with `mem_stash()`.
#'
#' @param ... Named objects to stash.
#' @return An environment containing the stashed objects. You can add
#'   objects to this environment in the usual fashion.
#'
#' @examples
#' # Supply named objects to stash
#' stash <- mem_stash(x = 1:3)
#'
#' # Access the stash like a regular environment
#' stash$x
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
#' * `root_ns_registry()` returns R's namespace registry as a list. It
#'   contains all the namespaces currently loaded in the R session.
#'
#' * `root_cpp11()` returns the precious list of cpp11. This is a
#'   doubly linked list preserved in R's own precious list.
#'
#' * `root_precious_list()` returns R's precious list. However this
#'   requires a custom build of R where `R_PreciousList` is exposed as a
#'   non-static symbol.
#'
#' @section The precious list:
#' The R precious list is (as currently implemented) a pairlist of
#' objects currently protected via the C API function
#' `R_PreserveObject()`. Preserved objects are stored in this pairlist
#' until a corresponding `R_ReleaseObject()` call is performed. Some
#' objects are meant to be preserved for the whole duration of the R
#' session (global caches), others are preserved for a limited
#' duration. It may happen that `R_ReleaseObject()` is not being
#' called when it should. This causes a memory leak via the precious
#' list.
#'
#' R currently does not provide an easy way to get the precious list.
#' So you will need to either patch R to expose it (e.g. remove its
#' `static` qualifier) so that you can call `root_precious_list()`, or
#' run R though a debugger like `gdb` or `lldb`.
#'
#' If you choose to use a debugger, use `p` to print the address of
#' the list:
#'
#' ```
#' p R_PreciousList
#' #> (SEXP) $0 = 0x000000010107cf58
#' ```
#'
#' Copy that address in an R string and dereference it with
#' [deref()]:
#'
#' ```
#' prec <- deref("0x000000010107cf58")
#' ```
#'
#' Avoid printing the precious list in the console because it contains
#' a large amount of data, such as the global cache for the global
#' environment and attached packages.
#'
#' Some things to consider while working with the precious list:
#'
#' - If you record a snapshot of the precious list, beware that
#'   objects in the global environment will be reachable through the
#'   global cache. These objects are normally excluded from
#'   snapshots. You can exclude an object from the snapshot by
#'   stashing it with [mem_stash()].
#'
#' - If you take before and after snapshots, make sure to capture the
#'   `R_PreciousList` address each time. The precious list is
#'   currently implemented as a stack of pairlist nodes. If you don't
#'   refresh the pointer you will miss all new elements added on the
#'   top of the stack.
#' @name roots
NULL

#' @rdname roots
#' @export
root_cpp11 <- function() {
  .Call(ffi_root_cpp11)
}
#' @rdname roots
#' @export
root_ns_registry <- function() {
  as.list(.Call(ffi_root_ns_registry))
}

the <- new_environment()

precious_list_source <- "
  #define R_NO_REMAP
  #include <Rinternals.h>

  extern SEXP R_PreciousList;

  [[cpp11::register]]
  SEXP precious_list() {
    return R_PreciousList;
  }
"
#' rdname roots
#' @export
root_precious_list <- function() {
  check_installed(c("cpp11", "decor"))

  if (is_null(the$precious_list)) {
    cpp11::cpp_source(code = precious_list_source)
    the$precious_list <- precious_list
  }

  the$precious_list()
}

jit_cache_source <- "
  #define R_NO_REMAP
  #include <Rinternals.h>

  extern SEXP JIT_cache;

  [[cpp11::register]]
  SEXP jit_cache() {
    return JIT_cache;
  }
"
jit_cache <- function() {
  check_installed(c("cpp11", "decor"))

  if (is_null(the$jit_cache)) {
    cpp11::cpp_source(code = jit_cache_source)
    the$jit_cache <- jit_cache
  }

  the$jit_cache()
}

#' Find all shortest or simple paths
#'
#' Wrappers around [igraph::all_shortest_paths()] and
#' [igraph::all_simple_paths()].
#'
#' @param snapshot A memory snapshot data frame created by
#'   [mem_snapshot()].
#' @param to,from Nodes from the `node` column of `snapshot`.  If
#'   `from` is not supplied, the paths from the root are taken by
#'   default. Can also be indices into `snapshot$node`.
#' @export
mem_paths_shortest <- function(snapshot, to, from = NULL) {
  mem_paths(snapshot, to, from, "shortest")
}
#' @rdname mem_paths_shortest
#' @export
mem_paths_simple <- function(snapshot, to, from = NULL) {
  mem_paths(snapshot, to, from, "simple")
}
mem_paths <- function(snapshot, to, from, op) {
  check_installed("igraph")
  arg_check_snapshot(snapshot)

  if (is_null(from)) {
    from <- snapshot$node[[1]]
  }
  to <- arg_as_mem_node(to, snapshot)
  from <- arg_as_mem_node(from, snapshot)

  i <- match(c(from$id, to$id), mem_id(snapshot))
  if (anyNA(i)) {
    abort("Can't find nodes in snapshot.")
  }
  if (length(i) != 2) {
    abort("Unexpected duplicate nodes in snapshot.")
  }

  graph <- mem_igraph(snapshot)

  paths <- switch(op,
    shortest = igraph::all_shortest_paths(graph, i[[1]], i[[2]])$res,
    simple = igraph::all_simple_paths(graph, i[[1]], i[[2]]),
    stop_internal("mem_paths", "Unexpected operation.")
  )

  node <- mem_node(snapshot)
  lapply(paths, lapply, function(x) node[[x]])
}
