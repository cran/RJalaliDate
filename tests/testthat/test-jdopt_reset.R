test_that("multiplication works", {

  factory_settings <- list(DEFAULT_SEPARATOR = "/"
                           , VALID_SEPARATORS = c("", "-", "/")
                           , MIN_YEAR = 1200L
                           , MAX_YEAR = 1500L)

  new_settings <- factory_settings
  new_settings$DEFAULT_SEPARATOR = "-"

  expect_equal(jdopt_reset(), factory_settings)

  expect_equal(jdopt_set_default_separator("-"), new_settings)
  expect_equal(jdopt_get_options()$DEFAULT_SEPARATOR, "-")
  expect_equal(jdopt_reset(), factory_settings)
  expect_equal(jdopt_get_options()$DEFAULT_SEPARATOR, "/")
  expect_equal(jdopt_get_options()$DEFAULT_SEPARATOR == "-", FALSE)
})
