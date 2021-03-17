local_options(
  memtools_verbose = FALSE
)

test_that("snapshot visits environments and functions", {
  e <- env(
    base_env(),
    x = quote(foo(bar)),
    y = env(base_env(), z = local(function() baz, base_env()))
  )
  objs <- list(
    e,
    e$x,
    e$y,
    e$y$z,
    quote(bar),
    quote(baz)
  )
  addrs <- sapply(objs, sexp_address)

  s <- mem_snapshot(e)
  expect_true(all(addrs %in% s$id))
})

test_that("stashes are not visited", {
  e1 <- new_environment()
  e2 <- new_environment()
  stash <- mem_stash(x = e1)
  s <- mem_snapshot(list(stash, e2))
  expect_false(sexp_address(e1) %in% s$id)
  expect_true(sexp_address(e2) %in% s$id)
})

test_that("numbers of parents, children, and retained nodes are computed", {
  x <- new_node(NULL, NULL)
  y <- new_node(NULL, NULL)
  z <- new_node(NULL, NULL)
  node_poke_car(x, y)
  node_poke_car(y, z)
  node_poke_car(z, x)

  s <- mem_snapshot(x)
  expect_equal(s$n_parents, rep(1, 3))
  expect_equal(s$n_children, rep(1, 3))
  expect_equal(s$n_retained, 2:0)
})

test_that("snapshot of Tarjan's graph is correct (1979)", {
  R <- alloc_list(3)
  I <- alloc_list(1)
  E <- alloc_list(1)
  K <- list(I, R)
  J <- list(I)
  H <- list(K, E)
  G <- list(I, J)
  F <- list(I)
  L <- list(H)
  D <- list(L)
  C <- list(F, G)
  A <- list(D)
  B <- list(A, D, E)
  list_poke(I, 0, K)
  list_poke(E, 0, H)
  list_poke(R, 0, A)
  list_poke(R, 1, B)
  list_poke(R, 2, C)

  s <- mem_snapshot(R)

  addrs <- sapply(list(R, A, B, C, D, E, F, G, H, I, J, K, L), sexp_address)
  addrs <- set_names(addrs, c("R", LETTERS[1:12]))

  locs <- set_names(match(addrs, s$id), names(addrs))
  nodes <- set_names(s$node[locs], names(addrs))

  expect_node <- function(node,
                          parents,
                          children,
                          dominator,
                          dominated) {
    node_parents <- purrr::map_chr(node$parents, ~ .x$from$id)
    node_children <- purrr::map_chr(node$children, ~ .x$to$id)
    node_dominated <- purrr::map_chr(node$dominated, ~ .x$id)
    expect_true(setequal(addrs[parents], node_parents))
    expect_true(setequal(addrs[children], node_children))

    if (is_null(dominator)) {
      expect_null(node$dominator)
    } else {
      expect_equal(node$dominator$id, addrs[[dominator]])
    }

    expect_true(setequal(addrs[dominated], node_dominated))
  }

  expect_node(
    nodes$R,
    parents = c("K"),
    children = c("A", "B", "C"),
    dominator = NULL,
    dominated = c("A", "B", "C", "D", "E", "H", "I", "K")
  )
  expect_node(
    nodes$A,
    parents = c("B", "R"),
    children = c("D"),
    dominator = "R",
    dominated = chr()
  )
  expect_node(
    nodes$B,
    parents = c("R"),
    children = c("A", "D", "E"),
    dominator = "R",
    dominated = chr()
  )
  expect_node(
    nodes$C,
    parents = c("R"),
    children = c("F", "G"),
    dominator = "R",
    dominated = c("F", "G")
  )
  expect_node(
    nodes$D,
    parents = c("A", "B"),
    children = c("L"),
    dominator = "R",
    dominated = c("L")
  )
  expect_node(
    nodes$E,
    parents = c("H", "B"),
    children = c("H"),
    dominator = "R",
    dominated = chr()
  )
  expect_node(
    nodes$F,
    parents = c("C"),
    children = c("I"),
    dominator = "C",
    dominated = chr()
  )
  expect_node(
    nodes$G,
    parents = c("C"),
    children = c("I", "J"),
    dominator = "C",
    dominated = c("J")
  )
  expect_node(
    nodes$H,
    parents = c("E", "L"),
    children = c("E", "K"),
    dominator = "R",
    dominated = chr()
  )
  expect_node(
    nodes$I,
    parents = c("F", "J", "G", "K"),
    children = c("K"),
    dominator = "R",
    dominated = chr()
  )
  expect_node(
    nodes$J,
    parents = c("G"),
    children = c("I"),
    dominator = "G",
    dominated = chr()
  )
  expect_node(
    nodes$K,
    parents = c("I", "H"),
    children = c("R", "I"),
    dominator = "R",
    dominated = chr()
  )
  expect_node(
    nodes$L,
    parents = c("D"),
    children = c("H"),
    dominator = "D",
    dominated = chr()
  )

  n_retained <- set_names(s$n_retained[locs], names(addrs))
  expect_equal(
    n_retained,
    c(
      R = 12,
      A = 0,
      B = 0,
      C = 3,
      D = 1,
      E = 0,
      F = 0,
      G = 1,
      H = 0,
      I = 0,
      J = 0,
      K = 0,
      L = 0
    )
  )
})

test_that("can add igraph structure", {
  skip_if_not_installed("igraph")

  s <- mem_snapshot(list(1, list(2)))
  g <- mem_igraph(s)
  expect_equal(igraph::gsize(g), 3)

  g <- attr(s, "mem_igraph")$igraph
  expect_equal(igraph::gsize(g), 3)
})

test_that("can take shortest paths", {
  skip_if_not_installed("igraph")

  x <- 1; y <- list(x); z <- list(y)
  s <- mem_snapshot(z)
  p <- mem_paths_shortest(s, 3)
  expect_length(p, 1)
  expect_equal(purrr::map_chr(p[[1]], "id"), s$id)

  x <- 1; y <- list(x); z <- list(y, x)
  s <- mem_snapshot(z)
  p <- mem_paths_shortest(s, sexp_address(x))
  expect_length(p, 1)
  expect_equal(purrr::map_chr(p[[1]], "id"), s$id[c(1, 3)])
})

test_that("can diff snapshots", {
  x <- new_node(1, NULL)
  before <- mem_snapshot(x)

  y <- new_node(2, x)
  after <- mem_snapshot(y)

  diff <- mem_diff(before, after)
  expect_equal(diff$type, c("pairlist", "double"))
})
