
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
#  holidayNYSE               Returns holidays for full-day NYSE calendar
################################################################################

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
holidayNYSE <-
    function(year = getRmetricsOptions("currentYear"), type = c("", "standard", "special"))
{
    # A function implemented by Diethelm Wuertz;
    # improved speed and handling of time zone by Yohan Chalabi;
    # special closings and argument 'type' incorporated by Georgi Boshnakov

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

    ##  Settings:
    type <- match.arg(type)
    ans <- NULL
    
    if(type == "" || type == "standard"){
        ## standard holidays
        ##
        ## Note by GNB at time of introducing argument 'type':
        ##     only change in this chunk is wrapping the old code in 'if'
        
        holidays <- character(0)
        
        ## Iterate years:
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
            if (y >= 2022) # GB, issue #6755
                holidays <- c(holidays, as.character(USJuneteenthNationalIndependenceDay(y)))
        }
        
        # Sort and Convert to 'timeDate':
        holidays <- sort(holidays)
        ans <- timeDate(format(holidays), zone = "NewYork", FinCenter = "NewYork")
        
        # Move Sunday Holidays to Monday:
        posix1 <- as.POSIXlt(ans, tz = "GMT")
        ans <- ans + as.integer(posix1$wday==0) * 24 * 3600
        
        # After July 3, 1959, move Saturday holidays to Friday
        # ... except if at the end of monthly/yearly accounting period
        # this is the last business day of a month.
        posix2 <- as.POSIXlt(as.POSIXct(ans, tz = "GMT") - 24 * 3600)
        y <- posix2$year + 1900
        m <- posix2$mon + 1
        calendar <- timeCalendar(y = y+(m+1)%/%13,
                                 m = m+1-(m+1)%/%13*12, d = 1,
                                 zone = "GMT", FinCenter = "GMT")
        lastday <- as.POSIXlt(calendar - 24*3600, tz = "GMT")$mday
        lon <- .last.of.nday(year = y, month = m, lastday = lastday, nday = 5)
        ExceptOnLastFriday <- timeDate(format(lon), zone = "NewYork",
                                       FinCenter = "NewYork")
        
        ans <- ans - as.integer(ans >= timeDate("1959-07-03",
                                zone ="GMT", FinCenter = "GMT") &
                                as.POSIXlt(ans, tz = "GMT")$wday == 6  &
                                (ans - 24*3600) != ExceptOnLastFriday ) * 24 * 3600
    }
    
    if(type == "" || type == "special"){
        ## special closings
        sp <- nyse_special_closings[as.integer(format(nyse_special_closings, format = "%Y")) %in% year]
        
        ans <- unique(sort(if(is.null(ans)) sp else c(ans, sp)))
    }

    # Remove Remaining Weekend Dates:
    posix3 <- as.POSIXlt(ans, tz = "GMT")
    ans <- ans[ !(posix3$wday == 0 | posix3$wday == 6)]

    # Return Value:
    ans
}


################################################################################

## GNB
## TODO: need care with time zones?
.reset_time <- function(x, hms) {
    timeDate(paste(format(x, format = "%Y-%m-%d"), hms))
}

## Author: Georgi N. Boshnakov
earlyCloseNYSE <- function(year) {

    year1 <- year >= 2012

    res <- timeDate()
    
    if(length(year1)) {
        ## Thanksgiving
        thanks <- USThanksgivingDay(year)
        ## this should be computed more robustly
        ## but the date is end of November, little risk for DST problems, etc.
        thanks_p1 <- thanks + 86400   ## 24 * 60 * 60  # always Friday
        
        ## indep <- USIndependenceDay(year) # July 4
        ## day before Independence day
        indep_m1 <- timeDate(paste0(year, "-07-03", " ", "13:00:00"))  # July 3
        
        indep_m1 <- indep_m1[!(dayOfWeek(indep_m1) %in% c("Sat", "Sun"))] 
        
        ## Christmas Eve
        xmas <- ChristmasDay(year)
        eve <- ChristmasEve(year)
        
        eve <- eve[!(dayOfWeek(xmas) %in% c("Mon", "Sat", "Sun"))]
        
        ## take care of empty vectors
        ## unfortunately, currently timeDate(character0)) gives a timeDate of length 1 (NA)
        res <- c(thanks_p1, indep_m1, eve)
        if(length(res) > 1 || length(res) == 1 && !is.na(res)) {
            res <- res[!is.na(res)]
            res <- .reset_time(res, "13:00:00")
            res <- sort(res)
        }
    }
    
    db_years <- (as.numeric(format(early_close, "%Y")))
    if(any(flags <- db_years %in% year))
        if(length(res) == 1 && is.na(res))
            ## res is empty
            res <- early_close[flags] ## assumed sorted in the db
        else
            res <- unique(sort(c(early_close[flags], res)))
    
    res
}
