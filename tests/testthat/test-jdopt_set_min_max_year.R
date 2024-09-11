test_that("multiplication works", {

  old_opt <- jdopt_get_options()
  expect_equal(old_opt$MIN_YEAR, 1200L)
  expect_equal(old_opt$MAX_YEAR, 1500L)

  new_opt <- old_opt
  new_opt$MIN_YEAR = 1000L
  new_opt$MAX_YEAR = 2000L

  expect_equal(jdopt_set_min_max_year(1000L, 2000L), new_opt)

  err <- "argument \"min_year\" is missing, with no default"
  expect_error(jdopt_set_min_max_year(), err)

  err <- "argument \"max_year\" is missing, with no default"
  expect_error(jdopt_set_min_max_year(1000L), err)

  err <- "Option value out of range. Allowed values are in \\[0, 9999\\]"
  expect_error(jdopt_set_min_max_year(1000L, 20000L), err)

  err <- "!is.na\\(min_year\\) is not TRUE"
  expect_error(jdopt_set_min_max_year(NA, 2000L), err)

  err <- "typeof\\(min_year\\) == \"integer\" is not TRUE"
  expect_error(jdopt_set_min_max_year(1000, 2000L), err)

  err <- "!is.na\\(max_year\\) is not TRUE"
  expect_error(jdopt_set_min_max_year(1000L, NA), err)

  err <- "typeof\\(max_year\\) == \"integer\" is not TRUE"
  expect_error(jdopt_set_min_max_year(1000L, 2000), err)

  err <- "max_year >= min_year is not TRUE"
  expect_error(jdopt_set_min_max_year(2000L, 1000L), err)

  err <- "length\\(min_year\\) == 1 & length\\(max_year\\) == 1 is not TRUE"
  expect_error(jdopt_set_min_max_year(integer(0), integer(0)), err)

  jdopt_reset()
})
