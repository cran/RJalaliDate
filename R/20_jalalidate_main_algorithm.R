# not exported
elements_to_double <- function(jyear, jmonth, jday){

  # preliminary tests
  stopifnot(length(jyear)==length(jmonth) & length(jmonth)==length(jday))
  i <- (is.na(jyear) | is.na(jmonth) | is.na(jday))
  jyear[i] <- NA ; jmonth[i] <- NA ; jday[i] <- NA
  if(all(is.na(jyear), is.na(jmonth), is.na(jday))) return(rep(NA, length(jyear)))

  base_year <- 1375
  distance <- jyear - base_year

  i <- (distance>=0) ; iPrime <- !i

  is_leap_year <- is_jalali_leap_year(jyear)

  months_count <- c(0, 31, 62, 93, 124, 155, 186, 216, 246, 276, 306, 336)
  days_2 <- months_count[jmonth] + (jday - 1) #month and day

  minus <- rep(365, length(jyear))
  minus[is_leap_year] <- minus[is_leap_year] + 1

  days_2[iPrime] <- minus[iPrime] - days_2[iPrime]

  days_1 <- rep(NA, length(jyear))
  distance[iPrime] <- -distance[iPrime] - 1

  cycle <- distance %/% 33
  remainder <- distance %% 33

  # stopifnot((cycle * 33 + remainder) == distance)

  remainder_added <- rep(NA, length(jyear))
  remainder_added <- ceiling(remainder / 4)
  remainder_added[iPrime] <- remainder_added[iPrime] -1

  remainder_added[remainder==0] <- 0

  days_1 <- (distance * 365) + (cycle * 8) + remainder_added

  days <- days_1 + days_2

  days[iPrime] <- days[iPrime] * -1

  days

}

# not exported
double_to_elements <- function(date_dbl){

  # preliminary tests
  stopifnot(is.double(date_dbl))
  year_out <- rep(NA_real_, length(date_dbl))
  month_out <- rep(NA_real_, length(date_dbl))
  day_out <- rep(NA_real_, length(date_dbl))
  if(all(is.na(date_dbl))) return(list(year = year_out
                                       , month = month_out
                                       , day = day_out))

  non_NA_index <- !is.na(date_dbl)
  date_dbl <- date_dbl[non_NA_index]

  base_year <- 1375

  # 33 years cycles: (33 * 365) + 8 = 12053
  year <- base_year + ((date_dbl %/% 12053) * 33)
  remainder33 <- date_dbl %% 12053

  yrs <- remainder33 %/% 365
  year <- year + yrs
  dys <- remainder33 %% 365
  leap_year_correction <- ceiling((yrs)/4)
  leap_year_correction[yrs==33] <- 8 #exception

  i <- (leap_year_correction > dys)
  year[i] <- year[i] - 1
  is_leap_years <- is_jalali_leap_year(year)
  dys[i] <- dys[i] + 365
  dys[i & is_leap_years] <- dys[i & is_leap_years] + 1

  dys <- dys - leap_year_correction

  month <- rep(0, length(date_dbl))
  day <- rep(0, length(date_dbl))

  i <- (dys >=186)
  dys[i] <- dys[i] - 186
  month[i] <- month[i] + 6 + (dys[i] %/% 30)
  day[i] <- dys[i] %% 30

  i <- !i
  month[i] <- (dys[i] %/% 31)
  day[i] <- dys[i] %% 31

  (month <- month + 1)
  (day <- day + 1)

  year_out[non_NA_index] <- year
  month_out[non_NA_index] <- month
  day_out[non_NA_index] <- day

  list(jyear = year_out, jmonth = month_out, jday = day_out)
}


