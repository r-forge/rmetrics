
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
# FUNCTION:                GENERATION OF TIMEDATE OBJECTS:
#  setClass                 'timeDate' S4 Class representation
#  setMethod inti           'initialize', 'timeDate
################################################################################

## ISO Date/Time Format:
.isoDate   <- "%Y-%m-%d"
.isoFormat <- "%Y-%m-%d %H:%M:%S"

setClass("timeDate",
         ## A class implemented by Diethelm Wuertz and Yohan Chalabi
         ## Modified by Georgi N. Boshnakov (added prototype)

         # Description:
         #   Class representatation for 'timeDate' Objects.

         # CLASS:
         slots = c(Data = "POSIXct",
                   format = "character",
                   FinCenter = "character"),
         prototype = list(Data = .POSIXct(numeric(0), tz = "GMT"),
                          format = "", # .isoFormat ?
                          FinCenter = "GMT"),
         validity = function(object) {
             if(!identical(attr(object@Data, "tzone"), "GMT"))
                 return("@Data must be in \"GMT\" timezone.")
             if(!is.numeric(unclass(object@Data)))
                 return("unclass(@Data) should be of class \"numeric\".")
             ## else TRUE
             TRUE
         })


setMethod("initialize", "timeDate", function(.Object, ...) {
    ## 2024-12-12 GNB: streamline somewhat; also drop the 'if(all(is.na(num)))'
    ##     clause since now 'format' is set to "" by callNextMethod(), see the
    ##     new 'prototype' in setClass() above
    .Object <- callNextMethod()

    ## if no arguments are passed in ..., do not try to define format of @Data
    if (...length()) {  # 2024-12-12 was: length(list(...))

        ## extract numerical value, drop time zone
        num <- c(unclass(.Object@Data))

        ## don't look for a format if @Data has only NA's
        ## 2024-12-12: was    if (!all(is.na(num)))
        if (.Object@format == "" && any(fin <- is.finite(num))) {
            ## convert - DST
            num <- .formatFinCenterNum(num, .Object@FinCenter, "gmt2any")

            ## check if num is a multiple of days
            ## GNB: the curious !(abs(.) > 0), instead of just . == 0, seems to be to cater for NA's
            ##      (-Inf %% 86400 gives NaN and abs(NaN) seems to give NA.
            ##      But NA and NaN are not guaranteed in arithmetic operations, see ?is.na
            ##   test <- !(abs(num %% 86400) > 0)
            ##   .Object@format <- ifelse(all(na.omit(test)), .isoDate, .isoFormat)
            ##
            ## This modification changes the format below

            ## test <- (num %% 86400) == 0
            ## .Object@format <- if(all(test[is.finite(test)])) .isoDate else .isoFormat
            ## NOTE: num here is different from the one used to compute 'fin',
            ## TODO: can .formatFinCenterNum change some fin to infinite?
            test <- all((num[fin] %% 86400) == 0)
            .Object@format <- if(test) .isoDate else .isoFormat
        }
    }
    .Object
})

################################################################################

## the classes below are by Georgi N. Boshnakov

setClass("timeInterval", slots = c(left = "timeDate", right = "timeDate"))

setMethod("initialize", "timeInterval", function(.Object, ...) {
    .Object <- callNextMethod()

    wrk <- .make_disjoint(.Object@left, .Object@right)

    .Object@left <- wrk[[1]]
    .Object@right <- wrk[[2]]

    .Object
})



## TODO: may uncomment in the future but may also change the internal representation
##
## timeIntervalVector <- setClass("timeIntervalVector", slots = c(left = "timeDate", right = "timeDate", strict = "logical"))
##
## timeIntervalList <- setClass("timeIntervalList", slots = c(left = "list", right = "list", strict = "logical"))
