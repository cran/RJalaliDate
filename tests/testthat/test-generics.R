test_that("multiplication works", {

  thursday <- stringi::stri_unescape_unicode("\\u067E\\u0646\\u062C\\u0020\\u0634\\u0646\\u0628\\u0647")
  far <- stringi::stri_unescape_unicode("\\u0641\\u0631\\u0648\\u0631\\u062F\\u06CC\\u0646")

  expect_equal(as.Date(JalaliDate(1)), as.Date("1996-03-21"))
  expect_equal(as.list(JalaliDate(1)), list(jyear=1375, jmonth=1, jday=2))
  expect_equal(as.character(JalaliDate(1)), "1375/01/02")

  res <- "13750102"
  expect_equal(as.character(JalaliDate(1), format="A", separator = ""), res)
  res <- "1375/01/02"
  expect_equal(as.character(JalaliDate(1), format="A"), res)
  res <- paste0(" 2 ", far, " 1375")
  expect_equal(as.character(JalaliDate(1), format="B"), res)
  res <- paste0(thursday, " 2 ", far, " 1375")
  expect_equal(as.character(JalaliDate(1), format="C"), res)

  # Thursday
  expect_equal(weekdays(JalaliDate(1)), thursday)

  # expect_equal(unclass(today.JalaliDate()), 10329) #
  expect_equal(diffdate(JalaliDate(10), JalaliDate(1)), 9)

  a <- JalaliDate(100)
  b <- JalaliDate(10)
  msg <- "This operation is not allowed!"

  expect_equal(a + 1, JalaliDate(101))
  expect_equal(1 + a, JalaliDate(101))
  expect_equal(a - 2, JalaliDate(98))
  expect_equal(a - b, 90)

  expect_error(2 - b, msg)
  expect_error(a + b, msg)
  expect_error(a * b, msg)
  expect_error(a * 2, msg)
  expect_error(a / b, msg)
  expect_error(a / 2, msg)
  expect_error(a %/% b, msg)
  expect_error(a ^ b, msg)
  expect_error(a %*% b, msg)
  # expect_error(!a, msg)
  # expect_error(a & b, msg)
  # expect_error(a | b, msg)

})
