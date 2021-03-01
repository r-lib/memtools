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
