
test_that("create new object", {

  a <- unclass(JalaliDate(0))
  expect_equal(a, 0)# 0 is equal 1375/01/01

  #arg: double
  expect_equal(unclass(JalaliDate(2:4)), c(2, 3, 4))
  #arg: Date
  expect_equal(as.character(JalaliDate(as.Date("2024-01-01"))), "1402/10/11")
  #arg: integer
  expect_equal(as.character(JalaliDate(2L)), "1375/01/03")
  #arg: decimal >> floor
  expect_equal(unclass(JalaliDate(1.5)), 1)
  #arg: NA
  expect_equal(unclass(JalaliDate(c(1, NA, NA_real_, 2))), c(1, NA, NA, 2))

  #arg: character
  expect_equal(unclass(JalaliDate(c("1375/01/01", NA))), c(0, NA))

  # with warning
  expect_warning(res <- as.character(JalaliDate(c("1375/01/03", NA, ""))), "NAs introduced by validation.")
  expect_equal(res, c("1375/01/03", NA, NA))

  expect_equal(as.character(JalaliDate(c(NA_character_, NA_character_, NA_character_))),
               as.character(c(NA, NA, NA)))

  # with warning
  expect_warning(res <- as.character(JalaliDate("99990/01/01")), "NAs introduced by validation.")
  expect_equal(res, NA_character_)

  # with warning
  expect_warning(res <- as.character(JalaliDate("9999/01/01")), "NAs introduced by validation.")
  expect_equal(res , NA_character_)

  expect_equal(as.character(JalaliDate(c("0000/01/01", "9999/10/15"), MAX_YEAR=9999, MIN_YEAR = 0))
               , c("0000/01/01", "9999/10/15"))

  tmp<- c("1375+01+01", "1390/02/02", "2000 02 02", "0100_02_02")
  # with warning
  expect_warning(res <- as.character(JalaliDate(tmp)), "NAs introduced by validation.")
  expect_equal(res, c(NA, "1390/02/02", NA, NA))

  expect_equal(as.character(JalaliDate(tmp, VALID_SEPARATORS=c("+", "_", " ", "/"), MAX_YEAR=9999, MIN_YEAR = 0)),
               c("1375/01/01", "1390/02/02", "2000/02/02", "0100/02/02"))


  #arg: list
  expect_equal(unclass(JalaliDate(list(1375, 1, 2))), 1)

  x <- list(y = c(1403, NA, 1402, 1400, 100), m=c(NA, 2, 3, 5, 10), d=c(8, 9, 10, 11, 40))
  expect_warning(res <- as.character(JalaliDate(x)) , "NAs introduced by validation.")
  expect_equal(res, c(NA, NA, "1402/03/10", "1400/05/11",NA))

  x <- list(y = c("a", NA, "1402", "1400"), m=c(NA, "2b", "3", "5"), d=c("m", "9c", "10a", 11))
  ekspr <- expression(as.character(JalaliDate(x)))
  cnd <- capture_warning(eval(ekspr))
  expect_equal(cnd$message, "NAs introduced by coercion")
  suppressWarnings(expect_equal(eval(ekspr), c(NA, NA, NA, "1400/05/11")))
  # Warning messages:
  # 1: In JalaliDate.list(y) : NAs introduced by coercion
  # 2: In JalaliDate.list(y) : NAs introduced by coercion
  # 3: In JalaliDate.list(y) : NAs introduced by coercion

  expect_warning(res <- as.character(JalaliDate(list(-10000,1,1))) , "NAs introduced by validation.")
  expect_equal(res, NA_character_)

  expect_equal(as.character(JalaliDate(list(-10000,1,1), MIN_YEAR=-10000)), "-10000/01/01")

  # The distant dates could be defined only in this way.
  x <- JalaliDate(list(c(100000, -100000),c(1, 2),c(10, 20)), MAX_YEAR=100000, MIN_YEAR=-100000)
  z <-  c("100000/01/10", "-100000/02/20")
  expect_equal(as.character(x), z)


})


test_that("unauthorized arguments", {

  expect_error(JalaliDate(FALSE), "This input is not allowed!") #arg: logical
  expect_error(JalaliDate(2+5i), "This input is not allowed!") # arg: complex
  expect_error(JalaliDate(), "This input is not allowed!") # no arg
  expect_error(JalaliDate(NA), "This input is not allowed!") # arg: NA

  expect_error(JalaliDate.default(1) , "This input is not allowed!") #

  # ~~~~~~~~~Below tests have different result in different OS.
  # mxv <- .Machine$double.xmax
  # msg <-
  # "Warning messages:
  #   1: In double_to_elements\\(x\\) :
  #   probable complete loss of accuracy in modulus
  # 2: In is_jalali_leap_year\\(year\\) :
  #   probable complete loss of accuracy in modulus"
  # res <- "492191765124503632728422606480602002620880602242442848662044648002826604408820824408864604664822064284084844062426688064082680228248066846820800286868608606666446880642222044262620288862004664646064064082080662008004042800682222466206604402042662866644600022244824602406446640046622022224246062226006248626/01/01"
  # suppressWarnings(expect_equal(as.character(JalaliDate(mxv)), res))
  # cnd <- capture_condition(as.character(JalaliDate(mxv)))
  #
  # # this is strange
  # mnv <- .Machine$double.xmin
  # expect_equal(as.character(JalaliDate(mnv)), "1375/01/01")
  #
  # x <- -100000000000000000000000000000000000000000000
  # ekspr <- expression(as.character(JalaliDate(x)))
  # res <- "-273790757487762421088466428688264806206268/01/01"
  # # eval(ekspr)
  # msg <-
  # "Warning messages:
  #   1: In double_to_elements\\(x\\) :
  #   probable complete loss of accuracy in modulus
  # 2: In is_jalali_leap_year\\(year\\) :
  #   probable complete loss of accuracy in modulus"
  # suppressWarnings(expect_equal(eval(ekspr), res))
  # cnd <- capture_condition(eval(ekspr))
  # expect_equal(cnd$message, "probable complete loss of accuracy in modulus")

})
