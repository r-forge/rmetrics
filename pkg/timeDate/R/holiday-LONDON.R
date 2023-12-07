
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 DESCRIPTION:
#  holidayLONDON             Returns holidays for British Bank Holidays
################################################################################

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
holidayLONDON <- function (year = getRmetricsOptions("currentYear")) {
    ## function implemented by Menon Murali;
    ## updated, corrected and refactored by GNB

    holidays <- list() # 2023-05-12 GNB was: NULL
    for (y in year) {
        if (y >= 1834 & y <= 1870) {
            # 1 May, 1 November, Good Friday and Christmas are the
            # only England Bank Holidays
            dts <- c(paste0(y, "-05-01"), paste0(y, "-11-01"))
            holidays <- list(holidays, dts, GoodFriday(y, ""),
                          ChristmasDay(y, ""))
        }
        if (y >= 1871) {
            holidays <- list(holidays,
                          ## as.character(GoodFriday(y)),
                          ## as.character(EasterMonday(y)),
                          ## as.character(GBSpringBankHoliday(y)),
                          ## as.character(GBSummerBankHoliday(y))

                          GoodFriday(y, ""),
                          EasterMonday(y, ""),
                          GBSpringBankHoliday(y, ""),
                          GBSummerBankHoliday(y, "")
                          )

            # Not entirely sure when Mon/Tue began to be given as
            # holiday when Christmas/Boxing Day fell on
            # Saturday/Sun. I'm assuming this was after the 1971
            # Banking and Financial Dealings Act which established the
            # new holiday schedule.
            ##
            ## GNB: In practice, this is so but see below.
            ##
            ##      The 1971 act declares 27th Dec to be bank holiday if 25th or
            ##      26th Dec is Sat or Sun but doesn't make provisions for 28th
            ##      Dec. But 28th Dec seems to have been declared BH by
            ##      Royal proclamations in all years ever since. ("seems"
            ##      because I haven't seen the proclamations and am not sure if
            ##      the calendars online for 40-50 years ago do not compute them
            ##      assuming that this is by definition)
            ##
            ## Christmas and Boxing Day - add following Monday and/or Tuesday if any falls in
            ##      the weekend
            ##
            ## GNB: the logic can be simplified by adding Christmas and Boxing days and the
            ##      Mon/Tue where necessary. Sat/Sun are omitted at the end anyway.
            holidays <- list(holidays, as.character(
                if (y < 1970) {
                    c(ChristmasDay(y), BoxingDay(y))
                } else {
                    posix1 <- as.POSIXlt(ChristmasDay(y))
                    switch(as.character(posix1$wday),
                           ## (posix1$wday == 0)  # Christmas on Sunday
                           "0" =  ChristmasDay(y) + (1 : 2) * 86400,
                           
                           ## (posix1$wday == 6) # Christmas on Saturday
                           "6" = ChristmasDay(y) + (2 : 3) * 86400,
                    
                           ## posix1$wday == 5) # Christmas on Friday
                           ##     the next Monday is a holiday
                           "5" = ChristmasDay(y) + c(0, 3) * 86400,
                           
                           ## default 1,2,3,4
                           c(ChristmasDay(y), BoxingDay(y))
                           )
            }))

            if (y >= 1974) {
                ## New Year's Day: if it falls on Sat/Sun, then is
                ## moved to following Monday
                posix1 <- as.POSIXlt(NewYearsDay(y))
                holidays <- list(holidays, 
                    if (posix1$wday == 0 | posix1$wday == 6) {
                        format..sdate(.on.or.after(y, 1, 1, 1))
                    } else {
                        NewYearsDay(y, "")
                    })
            }
            
            if (y >= 1978) {
                ## Early May Bank Holiday
                holidays <- list(holidays, GBEarlyMayBankHoliday(y, ""))
                
                ## special one-off bank holidays
                if (y == 1981) {
                    ## Royal wedding was a public holiday
                    dts <- paste0(y, "-07-29")
                    holidays <- list(holidays, dts)
                } else if (y == 1999) {
                    ## UK millenum day
                    dts <- format(GBMilleniumDay(), format = "%Y-%m-%d")
                    holidays <- list(holidays, dts)
                } else if (y == 2002) {
                    # Last Monday in May holiday moved to June 3, and
                    # Queen's Jubilee on June 4
                    dts <- list(# paste0(y, "-06-03"),
                             paste0(y, "-06-04"))
                    holidays <- list(holidays, dts)                
                } else if (y == 2011) {
                    ## Royal wedding declared a public holiday
                    dts <- paste0(y, "-04-29")
                    holidays <- list(holidays, dts)
                } else if (y == 2012) {
                    # Last Monday in May holiday moved to June 4, and
                    # Queen's Diamond Jubilee on June 5
                    dts <- list(# paste0(y, "-06-04"),
                             paste0(y, "-06-05"))
                    holidays <- list(holidays, dts)
                } else if (y == 2022) {
                    ## Last Monday in May (i.e., Spring Bank Holiday) holiday moved to June 2,
                    ## Unique Bank holidays:
                    ##     Queen's Diamond Jubilee.
                    ##     State Funeral of Queen Elizabeth II
                    dts <- list(# paste0(y, "-06-02"), # Thursday, Spring bank holiday
                             paste0(y, "-06-03"), # Friday, Platinum Jubilee bank holiday
                             paste0(y, "-09-19")  # Bank Holiday for the State Funeral of
                                                  # Queen Elizabeth II
                             )
                    holidays <- list(holidays, dts)
                } else if (y == 2023) {
                    ## Bank holiday for the coronation of King Charles III
                    dts <- paste0(y, "-05-08")
                    holidays <- list(holidays, dts)
                }
            }
        }
    }

#browser()    
    holidays <- unlist(holidays)
    holidays <- sort(holidays)
    ans <- timeDate(holidays, zone = "London", FinCenter = "Europe/London")
    posix1 <- as.POSIXlt(ans, tz = "GMT")
    ans[!(posix1$wday == 0 | posix1$wday == 6)] # Remove any Saturdays/Sundays
}

