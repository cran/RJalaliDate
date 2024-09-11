test_that("multiplication works", {

  expect_equal(is_gregorian_leap_year(c(1990, 1991, 1992, 1993, 1994, 1995))
               , c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))

  expect_error(is_gregorian_leap_year(c("1995"))
               , "non-numeric argument to mathematical function")

})
