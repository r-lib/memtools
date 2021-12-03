# must subset with a known field

    Code
      (expect_error(root$foobar))
    Output
      <error/rlang_error>
      Error in `$`: Must subset with a known node field.
      x Unknown field: `foobar`.

