test_that("dominance algorithm works", {
  # Example from the Tarjan 1976
  dfs <- list(
    R = c(K = 6L),
    A = c(R = 0L, B = 8L),
    D = c(A = 1L, B = 8L),
    L = c(D = 2L),
    H = c(L = 3L, E = 5L),
    E = c(H = 4L, B = 8L),
    K = c(H = 4L, I = 7L),
    I = c(K = 6L, F = 10L, G = 11L, J = 12L),
    B = c(R = 0L),
    C = c(R = 0L),
    F = c(C = 9L),
    G = c(C = 9L),
    J = c(G = 11L)
  )
  doms <- node_dominators(dfs)
  expect_equal(
    doms[c("node", "idom", "sdom")],
    tibble::tibble(
      node = c("R", "A", "D", "L", "H", "E", "K", "I", "B", "C", "F", "G", "J"),
      idom = c("R", "R", "R", "D", "R", "R", "R", "R", "R", "R", "C", "C", "G"),
      sdom = c("R", "R", "R", "D", "R", "R", "R", "R", "R", "R", "C", "C", "G"),
    )
  )

  # Same example, different DFS tree
  dfs <- list(
    R = c(K = 4L),
    C = c(R = 0L),
    F = c(C = 1L),
    I = c(F = 2L, G = 5L, J = 6L, K = 4L),
    K = c(I = 3L, H = 9L),
    G = c(C = 1L),
    J = c(G = 5L),
    B = c(R = 0L),
    E = c(B = 7L, H = 9L),
    H = c(E = 8L, L = 12L),
    A = c(B = 7L, R = 0L),
    D = c(A = 10L, B = 7L),
    L = c(D = 11L)
  )
  doms <- node_dominators(dfs)
  expect_equal(
    doms[c("node", "idom", "sdom")],
    tibble::tibble(
      node = c("R", "C", "F", "I", "K", "G", "J", "B", "E", "H", "A", "D", "L"),
      idom = c("R", "R", "C", "R", "R", "C", "G", "R", "R", "R", "R", "R", "D"),
      sdom = c("R", "R", "C", "R", "R", "C", "G", "R", "R", "R", "R", "B", "D"),
    )
  )

  dfs <- list(
    A = int(),
    B = c(A = 0L),
    C = c(B = 1L, A = 0L),
    D = c(C = 2L, E = 4L),
    E = c(D = 3L, H = 7L),
    F = c(C = 2L),
    G = c(F = 5L, B = 1L),
    H = c(G = 6L)
  )
  doms <- node_dominators(dfs)
  expect_equal(
    doms[c("node", "idom")],
    tibble::tibble(
      node = c("A", "B", "C", "D", "E", "F", "G", "H"),
      idom = c("A", "A", "A", "A", "A", "C", "A", "G")
    )
  )
})
