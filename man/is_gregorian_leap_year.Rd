% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/26_jalalidate_other_functions.R
\name{is_gregorian_leap_year}
\alias{is_gregorian_leap_year}
\title{check Gregorian leap year}
\usage{
is_gregorian_leap_year(year)
}
\arguments{
\item{year}{double}
}
\value{
logical
}
\description{
check if a Gregorian year is leap year
}
\examples{
is_gregorian_leap_year(c(2000, 2001, 2002))
# [1] TRUE FALSE FALSE

}
