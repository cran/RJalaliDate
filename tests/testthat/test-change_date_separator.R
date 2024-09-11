test_that("multiplication works", {

  expect_equal(change_date_separator(c("1401/01/02", "1402/03/04"), "-")
               , c("1401-01-02", "1402-03-04"))

  msg <- "change separator: The new separator does not belong to the set of valid separators!"
  expect_error(change_date_separator(c("1401/01/02", "1402/03/04"), "$")
               , msg)

  msg <- "change separator: The number of character of the separator must be 0 or 1!"
  expect_error(change_date_separator(c("1401/01/02", "1402/03/04"), "$$")
               , msg)


})
