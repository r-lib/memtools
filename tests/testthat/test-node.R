test_that("must subset with a known field", {
  root <- mem_snapshot(list(1))$node[[1]]
  expect_s3_class(root, "memtools_node")

  expect_snapshot({
    (expect_error(root$foobar))
  })
})

test_that("mem_node_dominated_ids() works", {
  e1 <- env(empty_env(), a = 1)
  e2 <- env(empty_env())
  e1$b <- e2

  s <- mem_snapshot(e1)
  root <- s$node[[1]]

  expect_null(root$dominator)

  root_dominated <- mem_node_dominated_ids(root)
  expect_equal(
    sort(root_dominated),
    sort(s$id[-1])
  )

  leaf <- s$node[[which(s$type == "double")]]
  expect_equal(
    leaf$dominated,
    list()
  )

  e2_node <- s$node[[which(s$type == "environment")[[2]]]]
  e2_dominated <- mem_node_dominated_ids(e2_node)
  expect_equal(
    e2_dominated,
    e2_node$children[[1]]$to$id
  )
})
