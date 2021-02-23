test_that("must subset with a known arrow field", {
  root <- mem_snapshot(list(1))$node[[1]]

  arrow <- root$children[[1]]
  expect_s3_class(arrow, "memtools_arrow")

  expect_snapshot({
    (expect_error(arrow$foobar))
  })
})
