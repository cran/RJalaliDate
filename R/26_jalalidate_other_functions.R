
#' list of a year weeks
#'
#' @description
#' create a data.frame consists start and end days of a year weeks
#'
#' @param year double
#' @param ... passing options ("MIN_YEAR" and/or "MAX_YEAR") to override package options (\link{jdopt_get_options})
#'
#' @return data.frame
#'
#' @examples
#' jalali_year_weeks(1402)
#'  #    week     f      l
#'  #1     1 1402/01/01 1402/01/04
#'  #2     2 1402/01/05 1402/01/11
#'  #..............................
#'  #52   52 1402/12/19 1402/12/25
#'  #53   53 1402/12/26 1402/12/29
#' @export
jalali_year_weeks <- function(year, ...){
  stopifnot(length(year)==1)
  year <- floor(year)

  local_opts <- settings::clone_and_merge(jdate_options, ...)
  stopifnot(year >= local_opts("MIN_YEAR"))
  stopifnot(year <= local_opts("MAX_YEAR"))

  yr <- paste0(stringr::str_pad(year, width = 4, side = "left", pad = "0"), "/01/01")
  Lyr <- paste0(stringr::str_pad(year + 1, width = 4, side = "left", pad = "0"), "/01/01")

  start_date <- as.Date(JalaliDate(yr , ...))
  wd <- lubridate::wday(start_date) + 1
  end_of_first_week <- start_date + (7-wd)

  second_week_f_day <- end_of_first_week + 1
  second_week_l_day <- end_of_first_week + 7

  sequenceVc <- seq(from=7, to= 366, by =7)

  first_days <- second_week_f_day + sequenceVc
  last_days <- second_week_l_day + sequenceVc

  first_days <- c(start_date, second_week_f_day, first_days)
  last_days <- c(end_of_first_week, second_week_l_day, last_days)

  jalali_first_days <- JalaliDate(first_days)
  jalali_last_days <- JalaliDate(last_days)

  years_weekDf <- data.frame(f = as.character(jalali_first_days), l = as.character(jalali_last_days))
  years_weekDf <- base::subset(years_weekDf, years_weekDf$f < Lyr)
  last_of_last_week <- JalaliDate(as.Date(JalaliDate(Lyr, ...))-1)

  N <- nrow(years_weekDf)
  years_weekDf[N,2] <- as.character(last_of_last_week)

  years_weekDf$week <- seq(1:N)
  years_weekDf[,c(3, 1, 2)]
  # years_weekDf[,c(1, 2, 3)]
}

# not exported
months <- function(jmonth){
  far <- "\\u0641\\u0631\\u0648\\u0631\\u062F\\u06CC\\u0646"
  ord <- "\\u0627\\u0631\\u062F\\u06CC\\u0628\\u0647\\u0634\\u062A"
  kho <- "\\u062E\\u0631\\u062F\\u0627\\u062F"
  tir <- "\\u062A\\u06CC\\u0631"
  mor <- "\\u0645\\u0631\\u062F\\u0627\\u062F"
  sha <- "\\u0634\\u0647\\u0631\\u06CC\\u0648\\u0631"
  meh <- "\\u0645\\u0647\\u0631"
  aba <- "\\u0622\\u0628\\u0627\\u0646"
  aza <- "\\u0622\\u0630\\u0631"
  dey <- "\\u062F\\u06CC"
  bah <- "\\u0628\\u0647\\u0645\\u0646"
  esf <- "\\u0627\\u0633\\u0641\\u0646\\u062F"

  mnths <- c (far, ord, kho, tir, mor, sha, meh, aba, aza, dey, bah, esf)
  (mnths <-  stringi::stri_unescape_unicode(mnths))
  mnths[jmonth]
}

#' check separator
#'
#' @description
#' Checks whether a separator can be among the set of valid separators
#'
#' @param separator character
#'
#' @return list of validation result and related message
#'
#' @examples
#' is_valid_separator("+")
#' # $result
#' # [1] TRUE
#' #
#' # $message
#' # [1] ""
#'
#' is_valid_separator("+/")
#' # $result
#' # [1] FALSE
#' #
#' # $message
#' # [1] "The number of character of the separator must be 0 or 1!"

#' @export
is_valid_separator <- function(separator){

  # separator <- NULL
  res <- TRUE; msg <- ""
  if (typeof(separator) != "character" || !all(class(separator) == "character")) {
    msg <- "The type or class of the separator is not valid!"
    res <- FALSE
  }else if (length(separator) != 1) {
    msg <- "The lenght of the separator must be 1!"
    res <- FALSE
  }else if (is.na(separator)) {
    msg <- "The separator could not be NA!"
    res <- FALSE
  }else if (nchar(separator) > 1) {
    msg <- "The number of character of the separator must be 0 or 1!"
    res <- FALSE
  }
  return(list(result = res, message = msg))
}

# not exported
is_valid_character_vector <- function(char_vector){
  res <- TRUE ; msg <- ""
  if (typeof(char_vector) != "character" | !all(as.character(class(char_vector)) == "character")) {
    msg <- "The date must be a one-dimensional character vector."
    res <- FALSE
  }

  return(list(result = res, message = msg))
}

#' validation JalaliDate elements
#'
#' @description
#'  check validation of Jalali date elements and returns results
#'
#' @param year double
#' @param month double
#' @param day double
#' @param ... passing options ("MIN_YEAR" and/or "MAX_YEAR") to override package options (\link{jdopt_get_options})
#'
#' @return list of validation result and related message
#' @details
#'  type of message are:
#' \itemize{
#'  \item{"y": year is not valid}
#'  \item{"m": month is not valid}
#'  \item{"d1": day must be between 0 to 31}
#'  \item{"d2": in the last 6 months of the year, the day should not be more than 30}
#'  \item{"d3": in leap years, the day should not be 30}
#' }
#'
#' @examples
#' is_valid_date_elements(c(1402, 1000), c(12, 13), c(10, 11), MIN_YEAR=100)
#' # $result
#' # [1]  TRUE FALSE
#' #
#' # $message
#' # [1] ""  "m"

#' @export
is_valid_date_elements <- function(year, month, day, ...){

  stopifnot(length(year)==length(month) & length(month)==length(day))
  stopifnot(all(is.double(year), is.double(month), is.double(day)))

  valid <- rep(TRUE, length(year))
  desc <- rep("", length(year))

  local_opts <- settings::clone_and_merge(jdate_options, ...)

  valid_year <- (year>= local_opts("MIN_YEAR") & year<= local_opts("MAX_YEAR"))
  valid[!valid_year] <- FALSE
  desc[!valid_year] <- "y"

  valid_month <- (month>0 & month<13)
  i <- which((!valid_month) & desc == "")
  valid[i] <- FALSE
  desc[i] <- "m"

  valid_day <- (day>0 & day<32)
  i <- which((!valid_day) & desc == "")
  valid[i] <- FALSE
  desc[i] <- "d1"

  i <- which(day>30 & month>6 & desc == "")
  valid[i] <- FALSE
  desc[i] <- "d2"

  ly <- !(is_jalali_leap_year(year)) #
  i <- which(day>29 & month==12 & ly & desc == "")
  valid[i] <- FALSE
  desc[i] <- "d3"

  (list(result = valid, message = desc))

}

#' validation JalaliDate character
#'
#' @description
#'  check validation of Jalali date in form of character and returns results
#'
#' @param date_char character
#' @param return_all_assessment_data logical , if it is FALSE only return validation result (logical vector)
#' @param ... passing options ("MIN_YEAR" and/or "MAX_YEAR") to override package options (\link{jdopt_get_options})
#'
#' @return list or logical, based on second argument
#'
#' @examples
#' is_valid_jalali_date_char("1402/10/15", FALSE)
#' # [1] TRUE
#'
#' @export
is_valid_jalali_date_char <- function(date_char, return_all_assessment_data = TRUE, ...){

  if ((temp_res<- is_valid_character_vector(date_char))$result == FALSE) {
    stop(paste0("Jalali date validation: ", temp_res$message), call. = FALSE)
  }

  if (length(date_char) > 0) {

    valid <- rep(TRUE, length(date_char))
    desc <- rep("", length(date_char))
    valid_x <- rep(as.character(NA), length(date_char))
    year <- rep(NA_real_, length(date_char))
    month <- rep(NA_real_, length(date_char))
    day <- rep(NA_real_, length(date_char))

    patrn <- jdate_pattern(...)
    valid_format <- stringr::str_detect(date_char, patrn)
    valid[which(!valid_format)] <- FALSE
    desc[which(!valid_format)] <- "f"

    if (length(which(valid_format))>0){ # if there are some jdate with valid format.

      jd_elementsLs <- jdate_elements(date_char, ...)
      year <- jd_elementsLs$year
      month <- jd_elementsLs$month
      day <- jd_elementsLs$day

      i <- (valid == TRUE & desc == "")

      checkLs <- is_valid_date_elements(year[i], month[i], day[i], ...)
      valid[i] <- checkLs$result
      desc[i] <- checkLs$message

      valid_x[which(valid)] <- date_char[which(valid)]
      year[!valid] <- NA_real_
      month[!valid] <- NA_real_
      day[!valid] <- NA_real_

    }
  }else{
    valid_x <- NA; valid=NA; desc=NA; year <- NA; month <- NA; day <- NA #if date_char is empty.
  }
  if(return_all_assessment_data){
    (output <- list(result = valid, message = desc, valid_x = valid_x, year = year, month = month, day = day))
  }else{
    (output <- valid)
  }

  return(output)
}


#' check Jalali leap year
#'
#' @description
#' check if a Jalali year is leap year
#'
#' @param year double
#'
#' @return logical
#'
#' @details
#' for details of calculation see \url{https://learn.microsoft.com/en-us/dotnet/fundamentals/runtime-libraries/system-globalization-persiancalendar}
#'
#' @examples
#' is_jalali_leap_year(c(1402, 1403, 1404))
#' # [1] FALSE  TRUE FALSE
#'
#' @export
is_jalali_leap_year <- function (year){

  year <- floor(year)

  (output <- rep(FALSE, length(year)))
  (temp <- year %% 33)
  (output[which(temp %in% c(1, 5, 9, 13, 17, 22, 26, 30))] <- TRUE)
  return(output)
}

#' check Gregorian leap year
#'
#' @description
#' check if a Gregorian year is leap year
#'
#' @param year double
#'
#' @return logical
#'
#' @examples
#' is_gregorian_leap_year(c(2000, 2001, 2002))
#' # [1] TRUE FALSE FALSE
#'
#' @export
is_gregorian_leap_year <- function(year){

  year <- floor(year)
  output <- rep(FALSE, length(year))

  output[which(((year%%4 == 0) & (year%%100 != 0)) | (year%%400 == 0))] <- TRUE

  output[which(is.na(year))] <- NA
  output
}

# not exported
jdate_pattern <- function(mode=1, ...){
  if (mode==1) { #for extracting elements of date

    local_opts <- settings::clone_and_merge(jdate_options, ...)

    seps <- local_opts("VALID_SEPARATORS")
    if(length(seps) == 0) stop("There is no valid separator.")
    patrnLs <- list()

    patrn <- paste0("^(\\d{4})()(\\d{2})()(\\d{2})$")
    if(!is.na(match("", seps))) patrnLs <- append(patrnLs, patrn)
    for (pt in seps) {
      if(pt != ""){
        patrn <- paste0("^(\\d{4})([", pt, "]{1})(\\d{2})([", pt, "]{1})(\\d{2})$")
        patrnLs <- append(patrnLs, patrn)
      }
    }

    if(length(patrnLs) == 0) stop("There is no valid pattern.")

    output <- ""
    for (item in patrnLs) {
      output <- paste0(output, "|", item)
    }
    output <- substring(output,2)
  }else if (mode==2){ # for changing of date separator
    patrn <- paste0("^(\\d{4})([:punct:]{1})(\\d{2})([:punct:]{1})(\\d{2})$"
                    ,"|"
                    ,"^(\\d{4})([:blank:]{1})(\\d{2})([:blank:]{1})(\\d{2})$"
                    ,"|"
                    ,"^(\\d{4})()(\\d{2})()(\\d{2})$")
    return(patrn)

  }else{
    stop("The requested pattern is not defiend!")
  }
}

# not exported
jdate_matrix <- function(date_char, pattern){
  # pattern <- patrn
  (result <- matrix(rep(NA, (length(date_char) * 6)), nrow=length(date_char), ncol = 6))

  (groupsAr <- stringr::str_match(date_char, pattern))
  (non_na_rows_indexVc <- which(!is.na(groupsAr[,1])))
  (non_na_rowsAr <- groupsAr[non_na_rows_indexVc,,drop=FALSE])
  (non_na_rows_trnpsAr <- t(non_na_rowsAr))
  (resVc <- non_na_rows_trnpsAr[which(!is.na(non_na_rows_trnpsAr))])
  (resAr <- matrix(resVc, nrow=6, ncol = ncol(non_na_rows_trnpsAr))) # 6: 1 + 5 section of date format
  (resAr <- t(resAr))

  (result[non_na_rows_indexVc,] <- resAr)

  return(result)

}

# not exported
jdate_elements <- function(date_char, ...){

  if ((temp_res<- is_valid_character_vector(date_char))$result == FALSE) {
    stop(paste0("Jalali date elements: ", temp_res$message), call. = FALSE)
  }

  patrn <- jdate_pattern(...)

  (elements <- jdate_matrix(date_char, patrn))

  (year <- floor(as.double(elements[,2])))
  (month <- floor(as.double(elements[,4])))
  (day <- floor(as.double(elements[,6])))

  (elementsLs = list(year=year, month=month, day=day))

  return(elementsLs)

}

#' change separator
#'
#' @description
#' change the Jalali date character separators. For invalid inputs, returns input without change
#'
#' @param date_char Jalali date (character)
#' @param new_separator character (valid separator)
#' @param ... passing options ("MIN_YEAR" and/or "MAX_YEAR") to override package options (\link{jdopt_get_options})
#'
#' @return Jalali date character
#'
#' @examples
#' change_date_separator(c("1350/01/02", "14021220", "1402/1/40"), "+", VALID_SEPARATORS=c("+"))
#' # [1] "1350+01+02" "1402+12+20" "1402/1/40"
#' @export
change_date_separator <- function(date_char, new_separator, ...) {
  # new_separator <- "+"

  local_opts <- settings::clone_and_merge(jdate_options, ...)

  if ((temp_res<- is_valid_character_vector(date_char))$result == FALSE) {
    stop(paste0("change separator: ", temp_res$message), call. = FALSE)
  }else if ((temp_res <- is_valid_separator(new_separator))$result==FALSE) {
    stop(paste0("change separator: ", temp_res$message), call. = FALSE)
  }else if ((is.na(match(new_separator, local_opts("VALID_SEPARATORS"))))){
    stop("change separator: The new separator does not belong to the set of valid separators!", call. = FALSE)
  }

  patrn <- jdate_pattern(mode = 2)

  (jd_matrix <- jdate_matrix(date_char, patrn))
  (jd_matrix[,3] <- new_separator)
  (jd_matrix[,5] <- new_separator)
  (na_rows_index <- which(is.na(jd_matrix[,1])))
  (jd_matrix[,1] = paste0(jd_matrix[,2],jd_matrix[,3],jd_matrix[,4],jd_matrix[,5],jd_matrix[,6]))
  (jd_matrix[na_rows_index,1] <- date_char[na_rows_index])
  return(as.character(jd_matrix[,1]))
}








