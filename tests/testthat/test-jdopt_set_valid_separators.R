test_that("multiplication works", {

  new_set <- c("/","/", "-",""," ","-","+","/")
  new_set2 <- sort(unique(new_set))

  old_opt <- jdopt_get_options()
  new_opt <- old_opt
  new_opt$VALID_SEPARATORS = new_set2

  testthat::expect_equal(jdopt_set_valid_separators(new_set), new_opt)

  err <- "!is.na\\(valid_separators\\) is not TRUE"
  expect_error(jdopt_set_valid_separators(NA), err)

  err <- "typeof\\(valid_separators\\) == \"character\" is not TRUE"
  expect_error(jdopt_set_valid_separators(c(2,3)), err)

  err <- "argument \"valid_separators\" is missing, with no default"
  expect_error(jdopt_set_valid_separators(), err)

  err <- "length\\(valid_separators\\) > 0 is not TRUE"
  expect_error(jdopt_set_valid_separators(character(0)), err)

  err <- "The separators must consist 0 or 1 character."
  expect_error(jdopt_set_valid_separators(c("/", "--")), err)

  err <- "After setting new valid separators, the default separator was changed automatically!"
  expect_message(jdopt_set_valid_separators("$"), err)

  jdopt_reset()

})
