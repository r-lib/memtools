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
