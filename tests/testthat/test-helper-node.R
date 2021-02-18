test_that("`dfs()` creates depth-first graph numbered in preorder", {
  n <- node("n")
  l2 <- node("l2", n = n)
  l3 <- node("l3", n = n)
  x <- node("l1", l2, n, l3)

  expect_equal(
    dfs(x),
    list(
      l1 = -1,
      l2 = c(l1 = 0),
      n = c(l2 = 1, l3 = 3, l1 = 0),
      l3 = c(l1 = 0)
    )
  )
})
