## This R package is free software; you can redistribute it and/or
## modify it under the terms of the GNU Library General Public
## License as published by the Free Software Foundation; either
## version 2 of the License, or (at your option) any later version.
##
## This R package is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU Library General Public License for more details.
##
## You should have received a copy of the GNU Library General
## Public License along with this R package; if not, write to the
## Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
## MA  02111-1307  USA


################################################################################
## FUNCTION:             DESCRIPTION:
##  timeNthNdayInMonth        Computes n-th ocurrance of a n-day in year/month
##  timeLastNdayInMonth       Computes the last n-day in year/month
################################################################################

timeNthNdayInMonth <-
function(charvec, nday = 1, nth = 1, format = "%Y-%m-%d",
    zone = "", FinCenter = "")
{
    ## A function implemented by Diethelm Wuertz
    ## Modified by GNB to use charvec@FinCenter if charvec is timeDate
    ##     and FinCenter is missing or default. Also, use attribute "tzone"
    ##     if the object is from another date-time class
    ##
    ##     The code worked with timeDate and other time-date objects before but
    ##     could give surprising results when arguments FinCenter and/or zone
    ##     were missing.

    ## Description:
    ##   Computes "nth" ocurrance of a "nday" (nth = 1,...,5)
    ##   in "year,month"

    ## Arguments:
    ##   charvec - a character vector of dates and times.
    ##   nday - an integer vector with entries ranging from
    ##       0 (Sunday) to 6 (Saturday).
    ##   nth - an integer vector numbering the n-th occurence.
    ##   format - the format specification of the input character vector.
    ##   FinCenter - a character string with the the location of the
    ##       financial center named as "continent/city".

    ## Value:
    ##   Returns the "nth" ocurrance of a "nday" (nth = 1,...,5)
    ##   in "year,month" as a 'timeDate' object.

    ## Example:
    ##   What date is the second Monday in April 2004?
    ##   timeNthNdayInMonth("2004-04-01", 1, 2)

    if (FinCenter == "")
        FinCenter <- if(is(charvec, "timeDate"))
                         charvec@FinCenter
                     else if(!is.null(attr(charvec, "tzone")))
                         attr(charvec, "tzone")
                     else
                         getRmetricsOptions("myFinCenter")
    if (zone == "")
        zone <- FinCenter

    ## timeDate:
    lt <- strptime(charvec, format, tz = "GMT")

    ## On or after:
    lt1 <- lt
    lt1$mday <- 1
    ## GNB: fix bug #1463 - the value returned may be in a different month, e.g.
    ##    > timeNthNdayInMonth("1996-06-23", 5, 4)
    ##    GMT
    ##      [1] [1996-07-19]
    ##
    ##      fix by Manny C - replace lt in julian.POSIXt(lt) by lt1
    ct <- 24*3600*(as.integer(julian.POSIXt(lt1)) + (nth-1)*7 + (nday-lt1$wday)%%7)
    timeDate(format(.POSIXct(ct), tz = "GMT"),
         format = format, zone = zone, FinCenter = FinCenter)
}


timeLastNdayInMonth <-
function(charvec, nday = 1, format = "%Y-%m-%d", zone = "",
    FinCenter = "")
{
    ## A function implemented by Diethelm Wuertz
    ## Modified by GNB to use charvec@FinCenter if charvec is timeDate
    ##     and FinCenter is missing or default. Also, use attribute "tzone"
    ##     if the object is from another date-time class
    ##
    ##     The code worked with timeDate and other time-date objects before but
    ##     could give surprising results when arguments FinCenter and/or zone
    ##     were missing.

    ## Description:
    ##   Computes the last "nday" in "year/month"

    ## Arguments:
    ##   charvec - a character vector of dates and times.
    ##   nday - an integer vector with entries ranging from
    ##       0 (Sunday) to 6 (Saturday).
    ##   format - the format specification of the input character vector.
    ##   FinCenter - a character string with the the location of the
    ##       financial center named as "continent/city".

    ## Value:
    ##   Returns the last "nday" in "year/month" as a 'timeDate'
    ##   object.

    ## Example:
    ##   What date has the last Tuesday in May, 1996?
    ##   timeLastNdayInMonth("1996-05-01", 2)

    if (FinCenter == "")
        FinCenter <- if(is(charvec, "timeDate"))
                         charvec@FinCenter
                     else if(!is.null(attr(charvec, "tzone")))
                         attr(charvec, "tzone")
                     else
                         getRmetricsOptions("myFinCenter")
    if (zone == "")
        zone <- FinCenter

    ## Last Day:
    last.day <- c(31,28,31, 30,31,30, 31,31,30, 31,30,31)
    lt <- strptime(charvec, format, tz = "GMT")
    y <- 1900 + lt$year
    leap.year <- (y%%4 == 0 & (y%%100 != 0 | y%%400 == 0))
    leap.day <- leap.year & (lt$mon == 1) # leap year *and* february -> 28 + 1 days
    lt$mday <- last.day[1 + lt$mon] + leap.day
    lt <- strptime(lt, format, tz = "GMT")
                ## to make sure that lt$wday
                ## represents the wday of the
                ## last day of the month

    ## GNB: this could return a wrong value, including in the following month,
    ##      before the change we had:
    ##      > timeLastNdayInMonth("1996-05-01", 2)
    ##      GMT
    ##      [1] [1996-06-04]
    ## the last Tue in May is apparantly in June! (from the doc. of the function!)
    ##
    ## cause:  (nday - lt$wday)%%7 gives unsitable values if (nday - lt$wday) != 0
    ##     replace  '+ (nday - lt$wday)%%7' with  '- (lt$wday - nday)%%7'
    ##
    ## was: 24*3600*(as.integer(julian.POSIXt(lt)) + (nday - lt$wday)%%7)
    ct <-   24*3600*(as.integer(julian.POSIXt(lt)) - (lt$wday - nday)%%7)
    timeDate(format(.POSIXct(ct), tz = "GMT"),
         format = format, zone = zone, FinCenter = FinCenter)
}


################################################################################
