test_that("multiplication works", {

  expect_equal(is_jalali_leap_year(c(1400, 1401, 1402, 1403, 1404, NA))
               , c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))

  expect_equal(is_jalali_leap_year(NA), FALSE)

  expect_error(is_jalali_leap_year()
               , "argument \"year\" is missing, with no default")

  expect_error(is_jalali_leap_year("1401")
               , "non-numeric argument to mathematical function")

})
