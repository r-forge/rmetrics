
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
    
    # Description:
    #   Returns the frequency of a timeDate vector
    
    # Arguments:
    #   x - an object of class timeDate
    
    # Example:
    #   frequency(timeCalendar())
    #   frequency(timeCalendar()[(1:3)*4])
    #   frequency(timeLastDayInQuarter(timeCalendar())[(1:3)*4])
    
    # FUNCTION:
    
    # Frequency:
    frequency <- 1
    if(isMonthly(x)) frequency <- 12
    if(isQuarterly(x)) frequency <- 4
    
    # Return Value:
    frequency
}

## ... but package timeSeries also defines S4 method and imports the S4 generic from
## timeDate. So for now keep the S4 method, as well.
##
## No, actually timeSeries imports 'frequency' from timeDate;
## TODO: something goes wwrong when timeSeries exports methods for 'frequency'
setMethod("frequency", "timeDate", frequency.timeDate)

################################################################################
