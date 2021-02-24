# must subset with a known field

    Code
      (expect_error(root$foobar))
    Output
      <error/rlang_error>
      Must subset with a known node field.
      x Unknown field: `foobar`.
      Backtrace:
       1. testthat::expect_error(root$foobar)
       8. memtools::`$.memtools_node`(root, foobar)

