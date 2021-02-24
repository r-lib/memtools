# must subset with a known arrow field

    Code
      (expect_error(arrow$foobar))
    Output
      <error/rlang_error>
      Must subset with a known arrow field.
      x Unknown field: `foobar`.
      Backtrace:
       1. testthat::expect_error(arrow$foobar)
       8. memtools::`$.memtools_arrow`(arrow, foobar)

