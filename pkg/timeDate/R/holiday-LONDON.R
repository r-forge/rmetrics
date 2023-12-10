
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

holidayLONDON <- function (year = getRmetricsOptions("currentYear")) {
    ## function implemented by Menon Murali;
    ## updated, corrected and refactored by GNB

    ## ignoring year <= 1833 as there wereno official holidays then
    y1834to1870 <- year[year >= 1834 & year <= 1870]
    y1871toInf <- year[year >= 1871]

    holidays <- list() # 2023-05-12 GNB was: NULL

    if(length(y1834to1870)) {
        ## 1 May, 1 November, Good Friday and Christmas are the
        ## only England Bank Holidays
        holidays <- list(holidays,
                         paste0(y1834to1870, "-05-01"),
                         paste0(y1834to1870, "-11-01"),
                         GoodFriday(y1834to1870, ""),
                         ChristmasDay(y1834to1870, ""))
    }

    if(length(y1871toInf)) {
        holidays <- list(holidays,
                         GoodFriday(y1871toInf, ""),
                         EasterMonday(y1871toInf, ""),
                         GBSpringBankHoliday(y1871toInf, ""),
                         GBSummerBankHoliday(y1871toInf, "")
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
        ## borderline 1969 needs checking
        y1871to1969 <- y1871toInf[flag1969 <- y1871toInf <= 1969]
        y1970toInf  <- y1871toInf[!flag1969]
        
        if(length(y1871to1969)) {
            holidays <- list(holidays,
                             ChristmasDay(y1871to1969, ""),
                             BoxingDay(y1871to1969, "")
                             )
        }

        if(length(y1970toInf)) {
            xmas <- ChristmasDay(y1970toInf)
            posix1 <- as.POSIXlt(xmas)
            wd <- as.character(posix1$wday)
            f <- function(x, y) {
                switch(x,
                       ## (posix1$wday == 0)  # Christmas on Sunday
                       "0" =  as.character(ChristmasDay(y) + (1 : 2) * 86400),
                       
                       ## (posix1$wday == 6) # Christmas on Saturday
                       "6" = as.character(ChristmasDay(y) + (2 : 3) * 86400),
                                   
                       ## posix1$wday == 5) # Christmas on Friday
                       ##     the next Monday is a holiday
                       "5" = as.character(ChristmasDay(y) + c(0, 3) * 86400),
                       
                       ## default 1,2,3,4
                       list(ChristmasDay(y, ""), BoxingDay(y, ""))
                       )
            }
            holidays <- list(holidays,
                             .mapply(f, list(wd, y1970toInf), NULL)
                             )
        }

        y1974toInf  <- y1970toInf[y1970toInf >= 1974]
        if(length(y1974toInf)) {
            ## New Year's Day: if it falls on Sat/Sun, then is
            ## moved to following Monday
            posix1 <- as.POSIXlt(NewYearsDay(y1974toInf))
            weekend <- posix1$wday == 0 | posix1$wday == 6
        
            holidays <- list(holidays,
                             format..sdate(.on.or.after(y1974toInf[weekend], 1, 1, 1)),
                             NewYearsDay(y1974toInf[!weekend], "")
                             )
        }

        ## Early May Bank Holiday year >= 1978
        holidays <- list(holidays,
                         GBEarlyMayBankHoliday(y1974toInf[y1974toInf >= 1978], ""),
                         specialHolidayGB(y1974toInf, "")
                         )
    }

    holidays <- sort(unlist(holidays))
    ans <- timeDate(holidays, zone = "London", FinCenter = "Europe/London")
    posix1 <- as.POSIXlt(ans, tz = "GMT")
    ans[!(posix1$wday == 0 | posix1$wday == 6)] # Remove any Saturdays/Sundays
}
