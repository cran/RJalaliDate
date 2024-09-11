test_that("multiplication works", {

  a <- c("1401/10/20", "1402/11/21")
  expect_equal(is_valid_jalali_date_char(a)
              , list(result = c(TRUE, TRUE), message = c("", ""), valid_x = a
                     , year = c(1401, 1402), month = c(10, 11), day = c(20, 21)))

  expect_equal(is_valid_jalali_date_char(a, return_all_assessment_data = F)
               , c(TRUE, TRUE))

  a <- c("1401/10/20", "1402/11/32")
  expect_equal(is_valid_jalali_date_char(a, return_all_assessment_data = F)
               , c(TRUE, FALSE))

  a <- "1401/13/20"
  expect_equal(is_valid_jalali_date_char(a)
               , list(result = FALSE, message = "m", valid_x = NA_character_
                      , year = NA_real_, month = NA_real_, day = NA_real_))

  a <- "1401/1020" # or "1401$10/20"
  expect_equal(is_valid_jalali_date_char(a)
               , list(result = FALSE, message = "f", valid_x = NA_character_
                      , year = NA_real_, month = NA_real_, day = NA_real_))

})
