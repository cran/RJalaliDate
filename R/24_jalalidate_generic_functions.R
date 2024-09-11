
#' data type conversion
#'
#' @description
#' convert JalaliDate to character, Date, or list
#'
#' @param x JalaliDate object
#' @param format character.
#' One of c("A", "B", "C") elements:
#' \itemize{
#'  \item{"A": simple, combining Jalali date with DEFAULT_SEPARATOR}
#'  \item{"B": (year) (month_name) (day)}
#'  \item{"C": (year) (month_name) (day) (day_of_week)}
#'  }
#' @param separator character.
#' One of VALID_SEPARATORS (see \link{jdopt_get_options}) that converts Jalali date elements to character
#' @param ...
#' \itemize{
#'  \item{"as.character": The ... argument is used to pass options that overrides current options (see examples).}
#'  \item{"other": future usage}}
#' @return character, Date, or list
#'
#' @examples
#' as.character(JalaliDate(1), separator= "+", VALID_SEPARATORS = c("+"))
#' @export
as.character.JalaliDate <- function(x, format="A", separator=jdate_options("DEFAULT_SEPARATOR"), ...){
  if (length(x)>0) {
    stopifnot(typeof(format)=="character")
    stopifnot(length(format)==1)
    stopifnot(nchar(format)==1)
    stopifnot(format %in% c("A", "B", "C"))

    local_opts <- settings::clone_and_merge(jdate_options, ...)

    if (format=="A") {
      stopifnot(typeof(separator)=="character")
      stopifnot(length(separator)==1)
      stopifnot(nchar(separator)<2)
      stopifnot(separator %in% local_opts("VALID_SEPARATORS"))
    }

    output <- rep(NA_character_, length(x))
    i <- !is.na(x)
    x <- x[i]
    elements <- as.list(new_JalaliDate(x))

    if (format=="A") {
      res <- paste0(stringr::str_pad(formatC(elements$jyear, format = "fg"), width = 4, side = "left", pad = "0"),
                      separator
                    , stringr::str_pad(elements$jmonth, width = 2, side = "left", pad = "0")
                    , separator
                    , stringr::str_pad(elements$jday, width = 2, side = "left", pad = "0"))

    }else if (format== "B") {
      res <- paste(""
                    , elements$jday
                    , months(elements$jmonth)
                    , format(elements$jyear, scientific=F)
                    , sep = " ")
    }else if (format == "C") {
      res <- paste(weekdays.JalaliDate(x)
                   , elements$jday
                   , months(elements$jmonth)
                   , format(elements$jyear, scientific=F)
                   , sep = " ")

    }

    output[i] <- res
    return(output)
  }else{
    stop("Input is emptly!")
  }
}

#' @rdname as.character.JalaliDate
#' @export
as.Date.JalaliDate <- function(x, ...){
  if(all(is.na(x))) return(as.Date(NA))
  as.Date(as.double(x + 9575))
}

#' @rdname as.character.JalaliDate
#' @export
as.list.JalaliDate <- function(x, ...) double_to_elements(x)

#' print
#'
#' @param x JalaliDate
#' @param ... for future usages
#'
#' @return display
#'
#' @examples
#' print(JalaliDate(1))
#' # [1] "1375/01/02"
#' @export
print.JalaliDate<- function(x, ...) print(as.character(x))

#' day of week
#'
#' @description
#' return the day of week in Persian
#' #'
#' @param x JalaliDate object
#' @param abbreviate not applicable in Persian language
#'
#' @return character
#'
#' @examples
#' weekdays(JalaliDate(1))
#' #[1] `r stringi::stri_unescape_unicode("\\u067E\\u0646\\u062C\\u0020\\u0634\\u0646\\u0628\\u0647")`
#' @export
weekdays.JalaliDate <- function(x, abbreviate=NULL){
  # first day of 1375 is wednesday
  remainder <- unclass(x) %% 7

  wed <- "\\u0686\\u0647\\u0627\\u0631\\u0634\\u0646\\u0628\\u0647"
  thu <- "\\u067E\\u0646\\u062C\\u0020\\u0634\\u0646\\u0628\\u0647"
  fri <- "\\u062C\\u0645\\u0639\\u0647"
  sat <- "\\u0634\\u0646\\u0628\\u0647"
  sun <- "\\u06CC\\u06A9\\u0634\\u0646\\u0628\\u0647"
  mon <- "\\u062F\\u0648\\u0634\\u0646\\u0628\\u0647"
  tue <- "\\u0633\\u0647\\u0020\\u0634\\u0646\\u0628\\u0647"
  days <- c(wed, thu, fri, sat, sun, mon, tue)
  days <- stringi::stri_unescape_unicode(days)
  days[remainder+1]
}

# yearweek <- function(x, ...) {
#   UseMethod("yearweek")
# }

#' number of week
#'
#' @description
#' It shows which week of the year the desired date is.
#'
#' @param x JalaliDate object
#' @param ... for future usage
#'
#' @return list of current and last week number and label
#'
#' @examples
#' yearweek(JalaliDate(321))
#' # $week_number
#' # [1] "47"
#' #
#' # $week_label
#' # [1] "75W47"
#' #
#' # $last_week_number
#' # [1] "46"
#' #
#' # $last_week_label
#' # [1] "75W46"
#' @export
yearweek <- function(x, ...) {

  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US")

  # x <- JalaliDate(NA_real_)
  # x <- JalaliDate(Sys.Date())
  (year <- (as.list(x))$jyear)

  (first_day_of_year <- paste0(year, "/01/01"))
  (first_day_of_year[which(is.na(year))] <- NA)

  (e <- as.Date(x))
  (s <- as.Date(JalaliDate(first_day_of_year)))

  day_count <- as.integer(round(difftime(e, s, units = "days"), 0)) + 1

  wd <- base::weekdays(s, abbreviate = T)
  # weekdays(Sys.Date()+0:6, abbreviate = T)
  week_dayVc = c("Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri")
  added_dayVc = c(0:6)

  day_count <- day_count + added_dayVc[match(wd, week_dayVc)]

  number_of_week <- day_count%/%7
  number_of_week[which(day_count%%7 > 0)] <- number_of_week[which(day_count%%7 > 0)] + 1
  label_of_week <-paste0(substring(x,3,4)
                         , "W"
                         , stringr::str_pad(number_of_week, width = 2, side = "left", pad = "0"))
  label_of_week[which(is.na(number_of_week))] <- NA

  number_of_last_week <- rep(NA, length(number_of_week))
  number_of_last_week_label <- rep(NA, length(number_of_week))

  number_of_last_week[which(number_of_week > 1)] <- number_of_week[which(number_of_week > 1)] - 1
  number_of_last_week_label <-
    paste0(substring(x,3,4)
           , rep("W", length(x))
           , stringr::str_pad(number_of_last_week, width = 2, side = "left", pad = "0"))

  number_of_last_week_label[which(is.na(number_of_last_week))] <- NA

  output <- list(week_number = stringr::str_pad(number_of_week, width = 2, side = "left", pad = "0")
               , week_label = label_of_week
               , last_week_number = stringr::str_pad(number_of_last_week, width = 2, side = "left", pad = "0")
               , last_week_label = number_of_last_week_label)

  Sys.setlocale("LC_TIME",old_locale)

  return(output)
}

#' today as Jalali
#' @description
#' return JalaliDate object of today
#'
#' @return JalaliDate object
#'
#' @examples
#' today.JalaliDate()
#' # [1] "1403/04/31"
#' @export
today.JalaliDate <- function() JalaliDate(Sys.Date())

#' distance of two JalaliDate
#'
#' @description
#' calculate distance of two JalaliDate, that is, subtracts values of two JalaliDate and
#' return
#'
#' @param x JalaliDate object
#' @param y JalaliDate object
#'
#' @return double
#'
#' @examples
#' x <- JalaliDate(c(1, 2))
#' y <- JalaliDate(c(10, 12, 13, 20, 50))
#' diffdate(x,y)
#' # [1]  -9 -10  NA  NA  NA
#' # Warning message:
#' #  In diff.JalaliDate(x, y) : The length of two vectors aren't equal!
#' @export
diffdate <- function(x, y){
  if (as.character(class(x)) != "JalaliDate" | as.character(class(y)) != "JalaliDate"){
    stop("Both of two argument must be JalaliDate!")
  }

  a <- unclass(x) ; b <- unclass(y)

  expr <- expression(warning("The length of two vectors aren't equal!", call. = FALSE))
  if(length(b) > length(a)){
    a <- c(a, rep(NA, (length(b) - length(a))))
    eval(expr)
  }else if(length(a) > length(b)){
    b <- c(b, rep(NA, (length(a) - length(b))))
    eval(expr)
  }

  a - b
}

#' Operators
#'
#' @description
#' Perform arithmetic operations
#'
#' @param x JalaliDate or numeric
#' @param y JalaliDate or numeric
#'
#' @return
#' JalaliDate
#'
#' @details
#' Only +, - operators work with JalaliDate objects in some cases. If each of the two
#' arguments are JalaliDate, the '-' operator calculates the distance of two dates (see \link{diffdate}).
#'
#' @examples
#' JalaliDate("1395/10/11") + 1
#' # [1] "1395/10/12"
#' JalaliDate("1403/08/10") - 367
#' # [1] "1402/08/08"
#' JalaliDate("1403/09/10") - JalaliDate("1403/08/10")
#' # [1] 30
# not exported directrly but exported by inheritance
Operators <- function(x, y){
  # no action
}

#' @inherit Operators
#' @export
`+.JalaliDate` <- function(x, y){
  if (!((as.character(class(x)) == "JalaliDate" & as.character(class(y))=="numeric") |
        (as.character(class(x)) == "numeric" & as.character(class(y))=="JalaliDate"))) {
    stop("This operation is not allowed!", call. = FALSE)
  }else{
    if (as.character(class(x))=="JalaliDate") {
      y <- floor(y)
    }else{
      x <- floor(x)
    }
    NextMethod()
  }
}

#' @inherit Operators
#' @export
`-.JalaliDate` <- function(x, y){
  if (as.character(class(x))!="JalaliDate" & as.character(class(y))=="JalaliDate") {
    stop("This operation is not allowed!", call. = FALSE)
  }else if (as.character(class(x))=="JalaliDate" & as.character(class(y))=="JalaliDate") {
    diffdate(x, y)
  }else{
    y <- floor(y)
    NextMethod()
  }
}

#' @inherit Operators
#' @export
`*.JalaliDate` <- function(x, y) stop("This operation is not allowed!", call. = FALSE)

#' @inherit Operators
#' @export
`/.JalaliDate` <- function(x, y) stop("This operation is not allowed!", call. = FALSE)

#' @inherit Operators
#' @export
`%%.JalaliDate` <- function(x, y) stop("This operation is not allowed!", call. = FALSE)

#' @inherit Operators
#' @export
`%/%.JalaliDate` <- function(x, y) stop("This operation is not allowed!", call. = FALSE)

#' @inherit Operators
#' @export
`^.JalaliDate` <- function(x, y) stop("This operation is not allowed!", call. = FALSE)

#' @inherit Operators
#' @export
`%*%.JalaliDate` <- function(x, y) stop("This operation is not allowed!", call. = FALSE)


