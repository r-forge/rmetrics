
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

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  holidayNYSE               Returns holidays for full-day NYSE calendar
################################################################################


holidayNYSE <-
    function(year = getRmetricsOptions("currentYear"))
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns 'timeDate' object for full-day NYSE holidays

    # Arguments:
    #   year - an integer variable or vector for the year(s)
    #       ISO-8601 formatted as "CCYY" where easter or
    #       easter related feasts should be computed.

    # Value:
    #   Returns the holiday calendar for the NYSE formatted as
    #   'timeDate' object.

    # Details:
    #   The "New York Stock Exchange" calendar starts from year 1885.
    #   The rules are listed at the web site http://www.nyse.com.

    # Example:
    #   > holiday.NYSE(2004)
    #   [1] "America/New_York"
    #   [1] [2004-01-01] [2004-01-19] [2004-02-16] [2004-04-09]
    #   [5] [2004-05-31] [2004-07-05] [2004-09-06] [2004-11-25]

    # FUNCTION:

    #  Settings:
    holidays <- NULL

    # Iterate years:
    for (y in year ) {
        if (y >= 1885)
            holidays <- c(holidays, as.character(USNewYearsDay(y)))
        if (y >= 1885)
            holidays <- c(holidays, as.character(USIndependenceDay(y)))
        if (y >= 1885)
            holidays <- c(holidays, as.character(USThanksgivingDay(y)))
        if (y >= 1885)
            holidays <- c(holidays, as.character(USChristmasDay(y)))
        if (y >= 1887)
            holidays <- c(holidays, as.character(USLaborDay(y)))
        if (y != 1898 & y != 1906 & y != 1907)
            holidays <- c(holidays, as.character(USGoodFriday(y)))
        if (y >= 1909 & y <= 1953)
            holidays <- c(holidays, as.character(USColumbusDay(y)))
        if (y >= 1998)
            holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
        if (y >= 1896 & y <= 1953)
            holidays <- c(holidays, as.character(USLincolnsBirthday(y)))
        if (y <= 1970)
            holidays <- c(holidays, as.character(USWashingtonsBirthday(y)))
        if (y > 1970)
            holidays <- c(holidays, as.character(USPresidentsDay(y)))
        if (y == 1918 | y == 1921 | (y >= 1934 & y <= 1953))
            holidays <- c(holidays, as.character(USVeteransDay(y)))
        if (y <= 1968 | y == 1972 | y == 1976 | y == 1980)
            holidays <- c(holidays, as.character(USElectionDay(y)))
        if (y <= 1970)
            holidays <- c(holidays, as.character(USDecorationMemorialDay(y)))
        if (y >= 1971)
            holidays <- c(holidays, as.character(USMemorialDay(y)))
    }

    # Sort and Convert to 'timeDate':
    holidays <- sort(holidays)
    ans <- timeDate(format(holidays), zone = "NewYork", FinCenter = "NewYork")

    # Move Sunday Holidays to Monday:
    ans <- ans + as.integer(as.POSIXlt(ans@Data)$wday==0) * 24 * 3600

    # After July 3, 1959, move Saturday holidays to Friday
    # ... except if at the end of monthly/yearly accounting period
    # this is the last business day of a month.
    posix <- as.POSIXlt(ans@Data - 24*3600)
    y <- posix$year + 1900
    m <- posix$mon + 1
    lastday <- as.POSIXlt((timeCalendar(y = y+(m+1)%/%13,
        m = m+1-(m+1)%/%13*12, d = 1) - 24*3600)@Data)$mday
    ExceptOnLastFriday <- timeDate(as.character(.last.of.nday(year = y,
        month = m, lastday = lastday, nday = 5)))
    ans <- ans - as.integer(ans >= timeDate("1959-07-03") &
        as.POSIXlt(ans@Data)$wday == 6  &
        (ans - 24*3600) != ExceptOnLastFriday ) * 24 * 3600

    # Remove Remaining Weekend Dates:
    ans <- ans[ !(as.POSIXlt(ans@Data)$wday == 0 |
        as.POSIXlt(ans@Data)$wday == 6)]

    # Return Value:
    ans
}


################################################################################
