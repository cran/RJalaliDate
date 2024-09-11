test_that("changing option defalut value of separator", {
  jdopt_set_default_separator("-")
  expect_equal(jdopt_get_options()$DEFAULT_SEPARATOR, "-")

  err <- "The separator does not belong to valid separators set."
  expect_error(jdopt_set_default_separator("*"), err)

  jdopt_reset()
})
