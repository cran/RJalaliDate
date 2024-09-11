test_that("multiplication works", {

  expect_equal(is_valid_separator("+"), list(result = TRUE, message = ""))
  expect_equal(is_valid_separator(""), list(result = TRUE, message = ""))

  msg <- "The number of character of the separator must be 0 or 1!"
  expect_equal(is_valid_separator(" +"), list(result = FALSE, message = msg))

  msg <- "The type or class of the separator is not valid!"
  expect_equal(is_valid_separator(2), list(result = FALSE, message = msg))
  expect_equal(is_valid_separator(NA), list(result = FALSE, message = msg))

  msg <- "The lenght of the separator must be 1!"
  expect_equal(is_valid_separator(c("+", "/")), list(result = FALSE, message = msg))

  msg <- "The separator could not be NA!"
  expect_equal(is_valid_separator(NA_character_), list(result = FALSE, message = msg))

  expect_error(is_valid_separator(), "argument \"separator\" is missing, with no default")
})
