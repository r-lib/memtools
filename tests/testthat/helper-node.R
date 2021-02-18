dfs <- function(x) {
  state <- env(
    i = 0L,
    names = chr(),
    parents = list(),
    seen = env()
  )
  dfs0(state, x, int())

  parents <- lapply(state$parents, `-`, 1L)
  set_names(parents, state$names)
}

dfs0 <- function(state, x, parent_i) {
  stopifnot(is_node(x))

  addr <- sexp_address(x)
  if (env_has(state$seen, addr)) {
    i <- env_get(state$seen, addr)
    parents <- state$parents[[i]]
    state$parents[[i]] <- c(parents, parent_i)
    return()
  }

  i <- state$i + 1L
  name <- node_name(x)

  state$i <- i
  state$seen[[addr]] <- i
  state$names <- c(state$names, name)
  state$parents <- c(state$parents, list(parent_i))

  for (child in as.list(x)) {
    dfs0(state, child, set_names(i, name))
  }
}

node <- function(label, ...) {
  xs <- list2(...)
  xs <- set_names(xs, sapply(xs, node_name))

  structure(
    env(empty_env(), !!!xs),
    class = c("memtools:::node", "environment"),
    label = label
  )
}
is_node <- function(x) {
  inherits(x, "memtools:::node")
}
node_name <- function(x) {
  attr(x, "label")
}
