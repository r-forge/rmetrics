#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:                 FINANCIAL TIME SERIES:
#  durations                 Computes durations from a 'timeSeries' object
#  durationsSeries           Deprecated, use durations
################################################################################


durations <-
function(x, trim = FALSE, units = c("secs", "mins", "hours"))
{   
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Computes durations from a financial price series

    # Arguments:
    #   x - a univariate or multivariate 'timeSeries' object or a
    #       numeric vector or matrix.
    #   trim - a logical flag, by default TRUE, the first missing
    #       observation in the return series will be removed.
    #   units - a character value or vector which allows to set the
    #       units in which the durations are measured

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:
    stopifnot(is(x, "timeSeries"))

    if (x@format == "counts")
        stop(as.character(match.call())[1],
             " is for time series and not for signal series.")

    # Positions and Durations:
    pos <- time(x)
    dur <- c(NA, diff(as.integer(difftime(pos, pos[1], units = units[1]))))

    # Data Matrix:
    ans <- timeSeries(data = dur, charvec = pos, units = "Duration")
    if (trim) ans <- ans[-1, ]

    # Return Series:
    ans
}


# ------------------------------------------------------------------------------


durationSeries <- 
function(...) 
{
    # Deprecated:
    .Deprecated("returns", "timeSeries")
    
    # Durations
    durations(...)
}
    

################################################################################
