# must subset with a known arrow field

    Code
      (expect_error(arrow$foobar))
    Output
      <error/rlang_error>
      Error in `$`: Must subset with a known arrow field.
      x Unknown field: `foobar`.

