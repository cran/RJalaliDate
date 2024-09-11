test_that("multiplication works", {

  expect_equal(is_valid_date_elements(c(1401, 1402), c(10, 11), c(20, 21)), list(result = c(TRUE, TRUE)
                                                                                 , message = c("", "")))
  expect_equal(is_valid_date_elements(NA_real_, NA_real_, NA_real_), list(result = TRUE
                                                                          , message = ""))

  expect_equal(is_valid_date_elements(14001, 10, 20), list(result = FALSE, message = "y"))
  expect_equal(is_valid_date_elements(1401, 13, 20), list(result = FALSE, message = "m"))
  expect_equal(is_valid_date_elements(1401, 10, 32), list(result = FALSE, message = "d1"))
  expect_equal(is_valid_date_elements(1401, 10, 31), list(result = FALSE, message = "d2"))
  expect_equal(is_valid_date_elements(1401, 12, 30), list(result = FALSE, message = "d3"))

  msg <- "all\\(is.double\\(year\\), is.double\\(month\\), is.double\\(day\\)\\) is not TRUE"
  expect_error(is_valid_date_elements(c(1401, 1402), c(10L, 11L), c(20, 21)), msg)
  expect_error(is_valid_date_elements(NA, NA, NA), msg)
  expect_error(is_valid_date_elements(NA, 10, 20), msg)
  expect_error(is_valid_date_elements(1400, NA, 20), msg)
  expect_error(is_valid_date_elements(1400, 10, NA), msg)

  msg <- "length\\(year\\) == length\\(month\\) & length\\(month\\) == length\\(day\\) is not TRUE"
  expect_error(is_valid_date_elements(c(1401, 1402), c(10), c(20, 21)), msg)

  msg <- "argument \"day\" is missing, with no default"
  expect_error(is_valid_date_elements(c(10, 11), c(20, 21)), msg)

  msg <- "argument \"month\" is missing, with no default"
  expect_error(is_valid_date_elements(c(20, 21)), msg)

  msg <- "argument \"year\" is missing, with no default"
  expect_error(is_valid_date_elements(), msg)

})
