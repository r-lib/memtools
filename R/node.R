#' Is an object a memtools node?
#' @param x An R object.
#' @export
is_memtools_node <- function(x) {
  inherits(x, "memtools_node")
}
check_memtools_node <- function(x, arg = substitute(x)) {
  if (!is_memtools_node(x)) {
    msg <- sprintf("`%s` must be a `memtools_node`.", as_string(arg))
    abort(msg)
  }
}

mem_node_size <- function(x) {
  .Call(c_ptr_mem_node_size, x)
}

#' Climb nodes until a fork
#'
#' @description
#' * `mem_node_fork()` climbs nodes from child to parent until a node
#'   with more than one parent is reached.
#'
#' * `mem_node_dominator_fork()` climbs nodes from dominated to
#'   dominator until a node with more than one parent is reached.
#'
#' @param node A memtools node.
#' @param quiet Whether to report how many nodes were climbed.
#' @seealso [mem_node_dominator_until()]
#' @export
mem_node_fork <- function(node, quiet = FALSE) {
  node_fork(node, quiet, function(x) x$parents[[1]]$from, "parent")
}
#' @rdname mem_node_fork
#' @export
mem_node_dominator_fork <- function(node, quiet = FALSE) {
  node_fork(node, quiet, function(x) x$dominator, "dominator")
}
node_fork <- function(node, quiet, climb, what) {
  stopifnot(is_memtools_node(node))

  i <- 0
  while (length(node$parents) == 1) {
    node <- climb(node)
    i <- i + 1
  }

  if (!quiet) {
    writeLines(sprintf("%s Climbed %d %s(s)", info(), i, what))
  }

  node
}

#' Reach dominator of interest
#'
#' `mem_node_dominator_ns()` climbs nodes from dominated to dominator
#' until a namespace is reached.
#'
#' @inheritParams mem_node_fork
#' @param .p A predicate function applied to the objects represented
#'   by dominator nodes.
#' @param ... Arguments passed to `.p`.
#' @return The dominator node for which `.p` returned true. `NULL` if
#'   none were found.
#' @seealso [mem_node_fork()] and [mem_node_dominator_fork()]
#' @export
mem_node_dominator_until <- function(.node, .p, ..., .quiet = FALSE) {
  stopifnot(is_memtools_node(.node))

  i <- 1
  while (!is_null(.node <- .node$dominator)) {
    if (.p(deref(.node), ...)) {
      if (!.quiet) {
        writeLines(sprintf("%s Climbed %d dominator(s)", info(), i))
      }
      return(.node)
    }

    i <- i + 1
  }

  NULL
}

#' Retrieve dominance properties
#'
#' @description
#' * `mem_node_undominated_parents()` returns the list of parent nodes
#' which are not in the retained subtree.
#'
#' * `mem_node_dominated_ids()` returns the ids of all nodes in the
#'   dominated subtree.
#'
#' @param node A memtools node.
#' @export
mem_node_undominated_parents <- function(node) {
  check_memtools_node(node)

  dominated <- mem_node_dominated_ids(node)
  parents_ids <- purrr::map_chr(
    node$parents,
    purrr::pluck,
    "from",
    "id"
  )

  node$parents[!parents_ids %in% dominated]
}
#' @rdname mem_node_undominated_parents
#' @export
mem_node_dominated_ids <- function(node) {
  check_memtools_node(node)

  ids <- chr()
  for (node in node$dominated) {
    ids <- c(ids, node$id, mem_node_dominated_ids(node))
  }

  ids
}


#' @export
print.memtools_node <- function(x, ...) {
  writeLines(sprintf("<memtools:node>"))
  writeLines(sprintf("id: \"%s\"", x$id))
  writeLines(sprintf("type: \"%s\"", x$type))
  writeLines(sprintf("parents: %s", pillar::obj_sum(x$parents)))
  writeLines(sprintf("children: %s", pillar::obj_sum(x$children)))
  writeLines(sprintf("dominator: <%s>", pillar::obj_sum(x$dominator)))
  writeLines(sprintf("dominated: %s", pillar::obj_sum(x$dominated)))
  writeLines(sprintf("self_size: %s", format(x$self_size)))
  writeLines(sprintf("retained_size: %s", format(x$retained_size)))
  writeLines(sprintf("gc_depth: %d", x$gc_depth))
}

#' @export
`$.memtools_node` <- function(x, i, ...) {
  i <- as_string(substitute(i))
  node_fields <- c(
    "id",
    "type",
    "self_size",
    "retained_size",
    "parents",
    "children",
    "dominator",
    "dominated",
    "gc_depth"
  )
  if (!i %in% node_fields) {
    abort(c(
      "Must subset with a known node field.",
      x = sprintf("Unknown field: `%s`.", i)
    ))
  }
  NextMethod()
}
