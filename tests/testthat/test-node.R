test_that("must subset with a known field", {
  root <- mem_snapshot(list(1))$node[[1]]
  expect_s3_class(root, "memtools_node")

  expect_snapshot({
    (expect_error(root$foobar))
  })
})
