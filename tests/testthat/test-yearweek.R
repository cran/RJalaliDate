test_that("multiplication works", {
  res <- list(week_number = c("01", "39"), week_label = c("75W01", "77W39")
              , last_week_number = c(NA, "38"), last_week_label = c(NA, "77W38"))
  expect_equal(yearweek(JalaliDate(c(1, 1000))) , res)
})
