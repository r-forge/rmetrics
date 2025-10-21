
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
# FUNCTION:               DESCRIPTION:
#  frequency,timeDate      Returns the frequency of a timeDate vector
################################################################################

## GNB: made it an S3 method
##        setMethod("frequency", "timeDate",
frequency.timeDate <- function(x, ...)
{
    # A function implemented by Diethelm Wuertz
    # Streamlined by GNB

    # Description:
    #   Returns the frequency of a timeDate vector

    # Arguments:
    #   x - an object of class timeDate

    # Example:
    #   frequency(timeCalendar())
    #   frequency(timeCalendar()[(1:3)*4])
    #   frequency(timeLastDayInQuarter(timeCalendar())[(1:3)*4])

    ## 2025-10-21 GNB: was:
    ##     frequency <- 1
    ##     if(isMonthly(x)) frequency <- 12
    ##     if(isQuarterly(x)) frequency <- 4
    ##
    ##     # Return Value:
    ##     frequency

    ## the order is essential, since by definition a quarterly (at most one
    ## value in each quarter) ts is also monthly (at most one value in each
    ## month) and daily (at most one value in each day); and a monthly one is
    ## daily, as well. A return value of 1, just indicates that the ts is
    ## neither monthly nor quarterly.
    if(isQuarterly(x))
        4
    else if(isMonthly(x))
        12
    else
        1
}

## ... but package timeSeries also defines S4 method and imports the S4 generic from
## timeDate. So for now keep the S4 method, as well.
##
## No, actually timeSeries imports 'frequency' from timeDate;
## TODO: something goes wrong when timeSeries exports methods for 'frequency'
##
## 2025-10-21 GNB: the above problems were resolved in timeSeries v4041.111
## (released in Sep 2024). Should be possible to remove the S4 method.
##
## setMethod("frequency", "timeDate", frequency.timeDate)

################################################################################
