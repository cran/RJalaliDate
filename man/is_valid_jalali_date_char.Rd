% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/26_jalalidate_other_functions.R
\name{is_valid_jalali_date_char}
\alias{is_valid_jalali_date_char}
\title{validation JalaliDate character}
\usage{
is_valid_jalali_date_char(date_char, return_all_assessment_data = TRUE, ...)
}
\arguments{
\item{date_char}{character}

\item{return_all_assessment_data}{logical , if it is FALSE only return validation result (logical vector)}

\item{...}{passing options ("MIN_YEAR" and/or "MAX_YEAR") to override package options (\link{jdopt_get_options})}
}
\value{
list or logical, based on second argument
}
\description{
check validation of Jalali date in form of character and returns results
}
\examples{
is_valid_jalali_date_char("1402/10/15", FALSE)
# [1] TRUE

}
