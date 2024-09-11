
# constructor-------------------------------------------

# not exported
new_JalaliDate <- function(x) {
  stopifnot(typeof(x) == "double")
  stopifnot(all(class(x)=="numeric"))
  x <- floor(x)
  structure(x, class = "JalaliDate")
}

# helper -----------------------------------------------

#' JalaliDate object constructor
#'
#' @description
#' Creates an instance of JalaliDate object by S3 system.
#'
#' @param x object (double, integer, Date, character, list)
#' list' argument could be named like JalaliDate(list(y=1375, m=1, d=2))
#' @param ...
#'  ... argument is used to pass options that overrides current options (see examples).
#'
#' @return JalaliDate object
#'
#' @details
#' JalaliDate object is designed as 'base::Date' to handle Jalali (solar Hijri) date that is calendar of
#' Iran and Afghanistan. Like Date, the JalaliDate information is stored in the form of a 'double'
#' and is converted to another data type when necessary using the corresponding algorithm. The base day
#' (value = 0) is "1375/01/01". Calculation of leap year is like Microsoft .Net method (33 years cycles).
#' If the argument value is not valid at the time of conversion, it will be replaced with NA and a message
#' will be sent in this regard (see examples).
#'
#' @examples
#'
#' JalaliDate(c(1, NA_real_, 2))
#' # [1] "1375/01/02" NA "1375/01/03"
#'
#' JalaliDate(as.Date("2024-01-01"))
#' # [1] "1402/10/11"
#'
#' JalaliDate(1.5)
#' # [1] "1375/01/02"
#'
#' JalaliDate(c("1375/01/01", NA))
#' # [1] "1375/01/01" NA
#'
#' # with warning
#' JalaliDate(c("1375/01/03", "1375/0201", ""))
#' # [1] "1375/01/03" NA NA
#' # Warning message:
#' #   NAs introduced by validation.
#'
#' # year is out of default options range (1200-1500)
#' JalaliDate(list(9998,1,1))
#' # [1] NA
#' # Warning message:
#' #   NAs introduced by validation.
#'
#' JalaliDate(c("1380/01/01", "9998/10/15"), MAX_YEAR=9999)
#' # [1] "1380/01/01" "9998/10/15"
#'
#' tmp<- c("1375+01+01", "1390/02/02", "2000 02 02", "0100_02_02")
#' JalaliDate(tmp, VALID_SEPARATORS=c("+", "_", " ", "/"), MAX_YEAR=9999, MIN_YEAR = 0)
#' # [1] "1375/01/01" "1390/02/02" "2000/02/02" "0100/02/02"
#' @export
JalaliDate <- function(x, ...) {
  UseMethod("JalaliDate")
}

#' @export
JalaliDate.default <- function(x, ...) {
  # cat("This is a generic function\n")
  stop("This input is not allowed!", call. = FALSE)
}

#' @export
JalaliDate.double <- function(x, ...) new_JalaliDate(x, ...)

#' @export
JalaliDate.integer <- function(x, ...) new_JalaliDate(as.double(x), ...)

#' @export
JalaliDate.character <- function(x, ...) {

  stopifnot(all(class(x)=="character"))

  output <- rep(NA_real_, length(x))

  i <- !is.na(x)
  if(!all(i==FALSE)){
    check_validity <- is_valid_jalali_date_char(x, ...)$result
    if(!all(check_validity)) warning("NAs introduced by validation.", call. = FALSE)
    x <- x[check_validity & i]

    res <- jdate_elements(x, ...)
    jdate_dbl <- elements_to_double(res$year, res$month, res$day)

    output[check_validity & i] <- jdate_dbl
  }

  new_JalaliDate(output)
}

#' @export
JalaliDate.Date <- function(x, ...) new_JalaliDate(unclass(x) - 9575)

#' @export
JalaliDate.list <- function(x, ...){
  stopifnot(class(x)=="list")
  if(length(x) < 3) stop("The list is not valid.")
  jyear <- as.double(x[[1]]) ; jmonth <- as.double(x[[2]]) ; jday <- as.double(x[[3]])
  if(length(jyear) != length(jmonth) | length(jmonth) != length(jday)) stop("List elements must have equal length.")

  jdate_dbl <- rep(NA_real_, length(x$jyear))

  i <- !(is.na(jyear) | is.na(jmonth) | is.na(jday))
  jyear[!i] <- NA_real_ ; jmonth[!i] <- NA_real_ ; jday[!i] <- NA_real_

  checkLs <- is_valid_date_elements(jyear, jmonth, jday, ...)

  if(!all(checkLs$result)){
    jyear[!checkLs$result] <- NA_real_ ; jmonth[!checkLs$result] <- NA_real_ ; jday[!checkLs$result] <- NA_real_
    warning("NAs introduced by validation.", call. = FALSE)
  }

  i <- (i & checkLs$result)

  jdate_dbl[i] <- elements_to_double(jyear[i], jmonth[i], jday[i])

  new_JalaliDate(jdate_dbl)
}
