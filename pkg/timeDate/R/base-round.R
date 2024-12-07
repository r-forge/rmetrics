
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
# METHODS:                   DESCRIPTION:
#  round.timeDate            Rounds objects of class 'timeDate'
#  trunc.timeDate            Truncates objects of class 'timeDate'
################################################################################

round.timeDate <-
function(x, digits = c("days", "hours", "mins", "secs", "months", "years")) {
    ## A function implemented by Diethelm Wuertz;
    ## modified by Yohan Chalabi and Georgi N. Boshnakov

    ## Get Units:
    units <- match.arg(digits)
    FinCenter <- finCenter(x)

    ## Use:
    lt <- round.POSIXt(as.POSIXlt(x, tz = "GMT"), units = units)
    ans <- timeDate(lt, zone = FinCenter, FinCenter = FinCenter)

    # Return Value:
    ans
}


trunc.timeDate <-
function(x, units = c("days", "hours", "mins", "secs", "months", "years"), ...) {
    ## A function implemented by Diethelm Wuertz
    ## modified by Yohan Chalabi and Georgi N. Boshnakov

    ## Get Units:
    units = match.arg(units)
    FinCenter <- finCenter(x)

    ## Sorting under GMT is not what we want!
    ## GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    ## lt = trunc.POSIXt(GMT@Data, units = units)
    ## ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)

    ## Use:
    lt <- trunc.POSIXt(as.POSIXlt(x, tz = "GMT"), units = units)
    ans <- timeDate(lt, zone = FinCenter, FinCenter = FinCenter)

    ## Return Value:
    ans
}


## timeCeiling - new function with methods
##     Author: Georgi N. Boshnakov
timeCeiling <- function(x, ...)
    UseMethod("timeCeiling")

## based on base::round.POSIXt
timeCeiling.POSIXt <-
function (x, units = c("days", "hours", "mins", "secs", "months", "years"), ...) {
    .set_x_to_l_or_u <- function(lx, ll, lu) {
        tx <- unclass(as.POSIXct(lx))
        tl <- unclass(as.POSIXct(ll))
        up <- 0 != (tx - tl)

        up <- !is.na(up) & up
        y <- ll
        y[up] <- lu[up]
        y
    }

    units <- match.arg(units)

    x <- as.POSIXlt(x)
    ll <- trunc.POSIXt(x, units = units)

    if (units == "months") {
        lu <- ll
        lu$mon <- lu$mon + 1L
        .set_x_to_l_or_u(x, ll, lu)
    }
    else if (units == "years") {
        lu <- ll
        lu$year <- lu$year + 1L
        .set_x_to_l_or_u(x, ll, lu)
    }
    else {
        ## TODO: does this work around DST changes? Maybe need special treatment for days
        ##                                          (and maybe for others, too)?
        lu <- trunc.POSIXt(as.POSIXct(x) +                             #  86400 = 60 * 60 * 24
                           switch(units, secs = 1, mins = 60, hours = 3600, days = 86400),
                           units = units)
        .set_x_to_l_or_u(x, ll, lu)
    }
}

timeCeiling.timeDate <-
function(x, units = c("days", "hours", "mins", "secs", "months", "years"), ...) {
    ## A function implemented by Georgi N. Boshnakov

    units = match.arg(units)
    FinCenter <- finCenter(x)

    lt <- timeCeiling.POSIXt(as.POSIXlt(x, tz = "GMT"), units = units)
    ans <- timeDate(lt, zone = FinCenter, FinCenter = FinCenter)

    ans
}

################################################################################
