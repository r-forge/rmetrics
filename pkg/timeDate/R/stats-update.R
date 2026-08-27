
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
# MEHODS:                   DESCRIPTION:
#  update.timeDate           Makes a 'timeDate' object unique
################################################################################

## TODO: for now update only zone nd/or FinCenter, maybe more in future
update.timeDate <- function(object, zone = NULL, FinCenter = zone, dst_gap = "+", ...) {
    ## A function Implemented by GNB

    if(!is.null(zone))
        object <- timeDate(format(object), zone = zone, FinCenter = FinCenter, dst_gap = dst_gap)
    else if(!is.null(FinCenter)) {
        if (FinCenter == "")
            FinCenter <- getRmetricsOptions("myFinCenter")
        object@FinCenter <- FinCenter
    }
    
    object
}


################################################################################
