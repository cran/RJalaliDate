
# install.packages("settings")

jdate_options <- settings::options_manager(DEFAULT_SEPARATOR = "/"
                                         , VALID_SEPARATORS = c("", "-", "/")
                                         , MIN_YEAR = 1200L
                                         , MAX_YEAR = 1500L
                                         , .allowed = list(
                                            # DEFAULT_SEPARATOR= settings::inlist(jdate_options("VALID_SEPARATORS")),
                                            MIN_YEAR = settings::inrange(min=0, max=9999)
                                           , MAX_YEAR = settings::inrange(min=0, max=9999)
                                         ))
#' return package options
#'
#' @description
#' get a list of the package options
#'
#' @return
#' options list that includes DEFAULT_SEPARATOR, VALID_SEPARATORS, MIN_YEAR, MAX_YEAR
#'
#' @details
#' Package Options have four parts: DEFAULT_SEPARATOR, VALID_SEPARATORS, MIN_YEAR, and
#' MAX_YEAR. Options are used in validation and type conversion. For example,
#' if 'VALID_SEPARATORS' part of options include c("/", " "), validation of "1390-01-01"
#' return FALSE, because separator of this Jalali date is "-" that does not belong to valid
#' separators set. By default, the conversion of "1000/10/11" to JalaliDate would be failed,
#' because year of Jalali date should be between 1200 and 1500. By setting 'DEFAULT_SEPARATOR'
#' to "_", the result of conversion of JalaliDate(1) to character will be "1375_01_02".
#'
#' @examples
#' jdopt_get_options()
#' # $DEFAULT_SEPARATOR
#' # [1] "/"
#' #
#' # $VALID_SEPARATORS
#' # [1] ""  "-" "/"
#' #
#' # $MIN_YEAR
#' # [1] 1200
#' #
#' # $MAX_YEAR
#' #
#' # [1] 1500
#' @export
jdopt_get_options <- function(){
  jdate_options()
}

#' reset options to initial values
#' @description
#' return options value to factory settings
#'
#' @inherit jdopt_get_options return
#'
#' @details
#' The initial values, or factory settings, are: DEFAULT_SEPARATOR = "/"
#' , VALID_SEPARATORS = c("", "-", "/"), MIN_YEAR = 1200L, and MAX_YEAR = 1500L
#'
#' @examples
#' res <- jdopt_reset()
#' res
#'
#' #$DEFAULT_SEPARATOR
#' #[1] "/"
#' #
#' #$VALID_SEPARATORS
#' #[1] ""  "-" "/"
#' #
#' #$MIN_YEAR
#' #[1] 1200
#' #
#' #$MAX_YEAR
#' #[1] 1500
#'
#' @export
jdopt_reset <- function() {
  invisible(settings::reset(jdate_options))
}

#' Specify the default separator
#'
#' @description
#' specifying one of valid separators as default separator
#'
#' @param separator character
#'
#' @details
#' The default separator (where initially is "/") has several uses.
#' For example, to print JalaliDate: JalaliDate(list(1375, 1, 2)) that display
#' "1375/01/02". Selected separator must belong to 'VALID_SEPARATORS' set, otherwise
#' an error would be raised.
#'
#' @inherit jdopt_get_options return
#'
#' @examples
#' JalaliDate(Sys.Date())
#' # [1] "1403/04/29"
#' jdopt_set_default_separator("-")
#' JalaliDate(Sys.Date())
#' # [1] "1403-04-29"
#'
#' @export
jdopt_set_default_separator <- function(separator){
  if(!(separator %in% jdate_options("VALID_SEPARATORS")))
    stop("The separator does not belong to valid separators set.", call. = FALSE)
  invisible(jdate_options(DEFAULT_SEPARATOR = separator))
}

#' setting range of valid year
#'
#' @description
#' Determining the minimum and maximum valid value of Jalali date year
#'
#' @param min_year integer
#' @param max_year integer
#'
#' @details
#' 'min_year' must be equal or lower than 'max_year' and both must be integer.
#' Minimum value of 'min_year' is 0 and maximum value of 'max_year' is 9999.
#'
#' @inherit jdopt_get_options return
#'
#' @examples
#' JalaliDate(list(1000,1,1))
#' # [1] NA
#' jdopt_set_min_max_year(100L, 2000L)
#' JalaliDate(list(1000,1,1))
#' # [1] "1000/01/01"
#'
#' @export
jdopt_set_min_max_year <- function(min_year, max_year){
  stopifnot(!is.na(min_year))
  stopifnot(typeof(min_year)=="integer")
  stopifnot(all(class(min_year)=="integer"))

  stopifnot(!is.na(max_year))
  stopifnot(typeof(max_year)=="integer")
  stopifnot(all(class(max_year)=="integer"))

  stopifnot(length(min_year)==1 & length(max_year)==1)
  stopifnot(max_year >= min_year)

  invisible(jdate_options(MIN_YEAR = min_year, MAX_YEAR = max_year))

}

#' setting new valid separators
#'
#' @description
#' changing existing set of valid separators and defining a new set
#'
#' @param valid_separators character
#'
#' @details
#' Argument of the function is a character vector that each of elements has length of 0 or 1.
#' After changing 'VALID_SEPARATORS', if current 'DEFAULT_SEPARATOR' doesn't belong to new
#' 'VALID_SEPARATORS', first element of new 'VALID_SEPARATORS' (after sorting) will be set as
#' 'DEFAULT_SEPARATOR' and a message will be displayed.
#'
#' @return options list or warning
#'
#' @examples
#' jdopt_reset()
#' res <- jdopt_set_valid_separators(c("+", "$"))
#'  #After setting new valid separators, the default separator was changed automatically!
#'
#' res
#' # $DEFAULT_SEPARATOR
#' # [1] "$"
#' #
#' # $VALID_SEPARATORS
#' # [1] "$" "+"
#' #
#' # $MIN_YEAR
#' # [1] 1200
#' #
#' # $MAX_YEAR
#' # [1] 1500
#'
#' @export
jdopt_set_valid_separators <- function(valid_separators){
  stopifnot(!is.na(valid_separators))
  stopifnot(typeof(valid_separators)=="character")
  stopifnot(all(class(valid_separators)=="character"))
  stopifnot(length(valid_separators) > 0)

  if (all(sort(unique(nchar(valid_separators))) %in% c(0,1)) != TRUE)
    stop("The separators must consist 0 or 1 character.")

  vs <- sort(unique(valid_separators))

  jdate_options(VALID_SEPARATORS = vs)

  ds <- jdate_options("DEFAULT_SEPARATOR")
  if(!(ds %in% vs)){
    jdate_options(DEFAULT_SEPARATOR = vs[1])
    message("After setting new valid separators, the default separator was changed automatically!")
  }

  jdate_options()

}


