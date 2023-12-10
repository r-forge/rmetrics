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
# FUNCTION:             DESCRIPTION:
#  timeDate              Creates a 'timeDate' object from given dates
#  setMethod("timeDate", "character",
#  setMethod("timeDate", "timeDate",
#  setMethod("timeDate", "POSIXt",
#  setMethod("timeDate", "Date",
#  setMethod("timeDate", "numeric",
#  setMethod("timeDate", "missing",
#  setMethod("timeDate", "ANY",
# FUNCTION:
#  .formatFinCenter      Internal called by timeDate
#  strptimeDate          Creates for character time stamps a 'timeDate' object
################################################################################


setGeneric("timeDate",
           function(charvec, format = NULL, zone = "", FinCenter = "", ...)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    # Description:
    #   Creates a "timeDate' object from a character vector

    # Arguments:
    #   charvec - a character vector of dates and times. Alternatively
    #       it may be a 'timeDate', a 'Date', or a 'POSIXt' object. In
    #       these cases the argument will be coerced into a character
    #       string or character vector.
    #   format - the format specification of the input character
    #       vector. If set to NULL autodetection will be tried.
    #   zone - the time zone or financial center where the data
    #       were recorded.
    #   FinCenter - a character string with the the location of
    #       the financial center named as "continent/city" where the
    #       data will be used.

    # Value:
    #   Returns a S4 object of class 'timeDate'.

    # Note:
    #   Changeover DST not yet fully implemented!

    # Examples:
    #   timeDate("2004-01-01")
    #   timeDate(c("2004-01-01", "2004-01-01"))
    #   timeDate("2004-01-01 00:00:00")
    #   timeDate("20040101")
    #   timeDate("200401011600")
    #   timeDate("20040101000000")
    #   timeDate("1/1/2004") # American format
    #   timeDate("2004-01-01", FinCenter = "GMT")
    #   timeDate("20040101", FinCenter = "GMT")
    #   td = timeDate("2004-01-01", FinCenter = "GMT"); timeDate(td)
    #   td = timeDate("20040101", FinCenter = "GMT"); timeDate(td)


    standardGeneric("timeDate")
}
)

setMethod("timeDate", "character",
          function(charvec, format = NULL, zone = "", FinCenter = "", dst_gap = "+")
{
    # Settings and Checks:
    if (zone == "")
        zone <- getRmetricsOptions("myFinCenter")
    if (FinCenter == "")
        FinCenter <- getRmetricsOptions("myFinCenter")

    # ISO Date/Time Format:
    isoDate   <- "%Y-%m-%d"
    isoFormat <- "%Y-%m-%d %H:%M:%S"

    # Autodetect Format :
    if (is.null(format))
        ## 2023-12-09 GNB:
        ##     whichFormat(charvec[1])
        ## fails if charvec[1] is NA, even if otherwise the strings are uniform,
        ## call whichFormat with the whole vector:
        format <- whichFormat(charvec)
    if (format %in% c("unknown", "counts")) #-> "counts" catch potential problems from timeSeries
        return(timeDate(NA, zone = zone, FinCenter = FinCenter))

    # Midnight Standard & conversion to isoFormat:
    ct <- midnightStandard2(charvec, format)
    
    ## Do conversion
    ## YC: .formatFinCenterNum faster than .formatFinCenter
    ## TS: using zone is correct (charvec is converted to GMT)
    num <- .formatFinCenterNum(unclass(ct), zone, type = "any2gmt", dst_gap = dst_gap)

    ## Manually create the POSIXct object:
    ## it is important to set manually the tzone flag,
    num <-
        if (getRversion() >= "2.12.0")
            .POSIXct(num, "GMT")
        else
            structure(num, class = c("POSIXt", "POSIXct"), tzone = "GMT")

    new("timeDate",
        Data = num,
        # Note format is automatically created in
        # initialize,timeDate-method
        FinCenter = as.character(FinCenter))
}
)


setMethod("timeDate", "timeDate",
    function(charvec, format = NULL, zone = "", FinCenter = "", dst_gap = "+")
{
    # Description:
    #   timeDate

    # if zone not provided, change only the FinCenter in charvec (timeDate)
    if (zone == "") {
        if (FinCenter != "")
            finCenter(charvec) <- FinCenter
        charvec
    } else {
        callGeneric(format(charvec), zone = zone, FinCenter = FinCenter, dst_gap = dst_gap)
    }
}
)


setMethod("timeDate", "POSIXt",
    function(charvec, format = NULL, zone = "", FinCenter = "")
{
    # Description:
    #   POSIXt

    if (!(zone %in% c("", "GMT", "UTC"))) {
        callGeneric(format(charvec), zone = zone, FinCenter = FinCenter)
    } else {

        # Since zone is not provided consider that charvec is in GMT
        charvec <- as.POSIXct(charvec)
        attr(charvec, "tzone") <- "GMT"

        # FinCenter
        if (FinCenter == "")
            FinCenter = getRmetricsOptions("myFinCenter")

        new("timeDate",
            Data = charvec,
            # Note format is automatically created in
            # initialize,timeDate-method
            FinCenter = as.character(FinCenter))
    }
}
)


setMethod("timeDate", "Date",
    function(charvec, format = NULL, zone = "", FinCenter = "", dst_gap = "+")
{
    # Description:
    #   Date

    if (!(zone %in% c("", "GMT", "UTC"))) {
        callGeneric(format(charvec), zone = zone, FinCenter = FinCenter, dst_gap = dst_gap)
    } else {

        # Since zone is not provided consider that charvec is in GMT
        charvec <- as.POSIXct(charvec)
        attr(charvec, "tzone") <- "GMT"

        # FinCenter
        if (FinCenter == "")
            FinCenter = getRmetricsOptions("myFinCenter")

        new("timeDate",
            Data = charvec,
            # Note format is automatically created in
            # initialize,timeDate-method
            FinCenter = as.character(FinCenter))
    }
})


setMethod("timeDate", "numeric",
    function(charvec, format = NULL, zone = "", FinCenter = "")
{
    # Description:
    #   numeric

    # DW: Modification of setMethod("timeDate", "numeric") to handle
    #   decimal like inputs (exactly that what "yearmon" does)

    if (!is.null(format) & (format == "%Y" || format == "yearmon" )) {
        # DW: Handles what is known as yearmon format
        # Example:     timeDate(2008+seq(0, 23, by = 1)/12, "yearmon")
        #   Quarterly: timeDate(2008+seq(2, 23, by = 3)/12, format = "%Y")
        # The next 4 lines are borrowed from Zeileis' yearmon()
        year <- floor(charvec + 0.001)
        month <- floor(12 * (charvec - year) + 1 + 0.5 + 0.001)
        dd.start <- as.Date(paste(year, month, 1, sep = "-"))
        # here we concentrate to the end of month date ...
        dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, "%d"))
        charvec <- as.POSIXct(dd.end, origin = "1970-01-01", tz = "GMT")
    } else {
        charvec <- as.POSIXct(charvec, origin = "1970-01-01", tz = "GMT")
    }

    callGeneric()
}
)


setMethod("timeDate", "missing",
    function(charvec, format = NULL, zone = "", FinCenter = "")
{
    # Description:
    #   missing

    callGeneric(Sys.time(), format, zone, FinCenter)
}
)


setMethod("timeDate", "ANY",
    function(charvec, format = NULL, zone = "", FinCenter = "", dst_gap = "+")
{
    # Description:
    #   ANY

    callGeneric(as.character(charvec), format, zone, FinCenter, dst_gap = dst_gap)
}
)


################################################################################

.formatFinCenterNum <-
function(num, FinCenter, type = c("gmt2any", "any2gmt"), dst_gap = c("+", "-", "NA", ""))
{
    ## A function implemented by Diethelm Wuertz and Yohan Chalabi.
    ##
    ## Modified by GNB to work correctly around DST changes and documented below
    ## to easy further maintenance. Also added options to treat non-existent DST
    ## times (gaps) at the time of switching to DST.
    ##
    ## Description:
    ##   Internal function used by function timeDate()
    ##
    ## When the clocks are moved forward (usually in spring) in a particular
    ## zone an hour is skipped. What to do if such a time is
    ## specified. Technically, it is NA. But using that as default is likely to
    ## cause more harm than good. A number of alternatives may be sensible in
    ## particular cases. The new argument 'dst_gap' offers the possibility to
    ## move it up or down by one hour, or to set it to NA.
    ##
    ## Let y be the time at time zone Z and t the corresponding UTC/GMT time.
    ## timeDate objects store t. We have
    ##     t = y - DST - GMToffset = y - offset
    ## and
    ##     y = t + DST + GMToffset = t + offset
    ## before the modification the function was essentially using
    ##     t = y - offset
    ##     y = t + offset
    ## but this lead to misterious shifts around DST changes, except for the
    ## meridians of London and Central Europe.
    ##
    ## For example, Bulgaria/Sofia is GMT+2 non-DST and GMT+3 during DST.  The
    ## switch to DST in 1983 was at "1983-03-27 01:00:00". so this time doesn't
    ## actually exist in that time zone.
    ## With timeDate v4021.106 (and earlier) we get
    ##
    ##     Sofia_to_DST_char <- c("1983-03-26 23:00:00",
    ##                            "1983-03-27 00:00:00", # change to DST; doesn't exist in Sofia DST
    ##                            "1983-03-27 01:00:00",
    ##                            "1983-03-27 02:00:00",
    ##                            "1983-03-27 03:00:00")
    ## 
    ##     Sofia_to_DST_test <- Sofia_to_DST_char
    ##     Sofia_to_DST_test[2] <- "1983-03-27 01:00:00" # gap to gap + 1 hour
    ##     
    ##     Sofia_to_DST <- timeDate(Sofia_to_DST_char, zone = "Sofia", FinCenter = "Sofia")
    ##     
    ##     > Sofia_to_DST
    ##     Sofia
    ##     [1] [1983-03-26 23:00:00] [1983-03-26 23:00:00] [1983-03-27 00:00:00]
    ##     [4] [1983-03-27 01:00:00] [1983-03-27 03:00:00]
    ##
    ## We see that 1am and 2am are wrong. 0am on this date doesn't exist, so it
    ## being moved back by an hour may be suitable in some circumstances. 23pm
    ## and 3am are ok.
    ##
    ## The underlying GMT times:
    ##
    ##     > Sofia_to_DST@Data
    ##     [1] "1983-03-26 21:00:00 GMT" "1983-03-26 21:00:00 GMT"
    ##     [3] "1983-03-26 22:00:00 GMT" "1983-03-26 23:00:00 GMT"
    ##     [5] "1983-03-27 00:00:00 GMT"
    ##
    ## Note that here 2am GMT is the time of switch to DST. We see that the
    ## GMT's are correct (with the caviat for 0am Sofia) but the conversion for
    ## 22pm and 23pm GMT adds the non-DST GMToffset (+2), instead of GMT+3.
    ##    
    ## The source of the errors is revealed by writing the formulas so that they
    ## show what is a function of what.
    ##
    ## Let y be the time at time zone Z and t the corresponding UTC/GMT time
    ## (both in seconds since some origin, typically "1970-01-01").
    ## "timeDate" objects store t. We have
    ##
    ##     t = y - DST(y) - GMToffset(y) = y - offset(y)
    ## and
    ##     y = t + DST(y) + GMToffset(y) = t + offset(y)
    ##
    ## Time zone tables normally contain y paired with offset(y), so computing t
    ## from y is straightforward. But for computing y from t we need offset(y),
    ## not offset(t). GMToffset(y) typically is fixed at a given place (although
    ## changes do happen) but even so, DST(y) may not be the same as DST(t) at
    ## and just after the switch of DST (on or off).
    
    if (FinCenter == "GMT" || FinCenter == "UTC")
        return(num)

    type <- match.arg(type)
    signum <- switch(type,
                     "gmt2any" = +1,
                     "any2gmt" = -1)
    ##  otherwise give error

    dst_gap <- match.arg(dst_gap)

    # Get the DST list from the database:
    try <- try(dst.list <- rulesFinCenter(FinCenter), silent = TRUE)
    if (inherits(try, "try-error"))
        stop(gettextf("'%s' is not a valid FinCenter.", FinCenter))

    offSetIdx <- findInterval(num, dst.list$numeric)
    offSetIdx[offSetIdx < 1] <- 1 # consider first DST rule if event occured before

    ## GNB: add check for non-existent times;
    ##      assuming DST skips some time interval.

    if(signum == -1) { # any2gmt
        ## TODO: Check if this resolves cases like BDST in UK during WW2
        shifted_offset <- offSetIdx

        indx <- which(dst.list$isdst[offSetIdx] == 1)
        if(length(indx) > 0  &&  (dst_gap == "+" || dst_gap == "NA")) {
            ## GNB: check for non-existent DST times at FinCenter
            ##
            ## TODO: handle the case when offSetIdx contains 1
            ## DONE 2023-12-06 handle the case when offSetIdx contains 1
            ##    was giving error, eg for
            ##        holidayLONDON(1915:1917)
            ##    but not for the individual years, these were ok:
            ##        holidayLONDON(1915)
            ##        holidayLONDON(1916)
            ##        holidayLONDON(1917)
            ## 
            ## TODO: check the fix? I have somewhat forgotten the details
            offSetIdx_minus1 <- offSetIdx - 1
            if(any(offSetIdx_minus1 == 0))
               offSetIdx_minus1[offSetIdx_minus1 == 0] <- 1

            #dst_skip <- dst.list$offSet[offSetIdx][indx] - dst.list$offSet[offSetIdx - 1][indx]
            #adjust <- dst.list$offSet[offSetIdx][indx]
            adjust <- dst.list$offSet[offSetIdx][indx] -
                          dst.list$offSet[offSetIdx_minus1][indx]
            #adjust <- 3600

            wrk <- num[indx] - adjust
            shifted_offset[indx] <- findInterval(wrk, dst.list$numeric)
            shifted_offset[shifted_offset < 1] <- 1
            changed <- shifted_offset != offSetIdx

            if(dst_gap == "+") {
                #offSetIdx[changed] <- shifted_offset[changed]
                ## TODO: 3600? better to choose suitable elements of adjust
                #num[changed] <- num[changed] + dst.list$offSet[offSetIdx - 1][indx]
                #num[indx] <- num[indx] + 3600 # adjust[indx][changed] #dst_skip
                num[changed] <- num[changed] + 3600 # adjust[indx][changed] #dst_skip
            } else # dst_gap == "NA"
                num[changed] <- NA
        }
        num - dst.list$offSet[offSetIdx]
    } else { ## gmt2any
        ## GNB: this was just
        ##          num + dst.list$offSet[offSetIdx]
        ##      but this is incorrect around DST changes, since the offset offSetIdx
        ##      may need correction, particularly evifdent for time zones further from GMT,
        ## 
        ##
        res <- num + dst.list$offSet[offSetIdx]

        offset_after <- findInterval(res, dst.list$numeric)
        offset_after[offset_after < 1] <- 1

        offset_diff <- dst.list$offSet[offset_after] - dst.list$offSet[offSetIdx]

        res[offset_diff > 0] <- res[offset_diff > 0] + 3600 

        flag_dst_special <- num + 3600 <= dst.list$numeric[offset_after]
        
        ## ambiguous t change from DST to non-DST; consider DST
        res[offset_diff < 0 & flag_dst_special] + 3600
        res[offset_diff < 0 & !flag_dst_special] <- res[offset_diff < 0] - 3600
        
        res
    }
}


## 2023-01-08 GNB: removed .formatFinCenter() since no longer used.
##
## .formatFinCenter <-
## function(charvec, FinCenter, type = c("gmt2any", "any2gmt"))
## {
##     # A function implemented by Diethelm Wuertz
##     #   thanks to contributions from Martin Maechler
## 
##     # Description:
##     #   Internal function used by function timeDate()
## 
##     if (FinCenter == "GMT" || FinCenter == "UTC")
##         return(charvec)
## 
##     ## else start working:
##     type <- match.arg(type)
##     signum <- switch(type,
##                      "gmt2any" = +1,
##                      "any2gmt" = -1)
##     ##  otherwise give error
## 
## 
##     # Get the DST list from the database:
##     try <- try(dst.list <- rulesFinCenter(FinCenter), silent = TRUE)
##     if (inherits(try, "try-error"))
##         stop(gettextf("'%s' is not a valid FinCenter.", FinCenter))
##     # Update list with last entry:
##     z = as.matrix(dst.list)
##     z[dim(z)[1], ]
##     vec1 = as.vector(c(z[, 1], "2099-01-01 00:00:00"))
##     vec2 = as.vector(c(z[, 2], rev(z[, 2])[1]))
##     dst.list = data.frame(ruleChanges = as.character(vec1),
##     offSet = as.integer(vec2))
##     # Extract the dates when DST was changed:
##     dst.dates = as.character(dst.list[, 1])
##     # Extract the Offsets to GMT
##     dst.offsets = as.character(dst.list[, 2])
##     # The new dates ar the charvec's:
##     new.dates = charvec
##     # The new offsets are still unknown:
##     new.offsets = rep(NA, length(charvec))
##     # Combine all Dates and Offsets:
##     dates = c(dst.dates, new.dates)
##     offsets = c(dst.offsets, new.offsets)
##     # The number of Offsets:
##     n = length(dates)
##     # Order the Dates:
##     o = order(dates)
##     # Dates and Offsets in the right order:
##     o.dates = dates[o]
##     o.offsets = offsets[o]
##     # The points at which we have to determine the offsets
##     xout = (1:n)[is.na(o.offsets)]
##     # The date indexes:
##     x = (1:n)[-xout]
##     # The corresponding offsets
##     y = o.offsets[x]
##     # The new offsets:
##     yout = approx(x, y , xout, method = "constant")$y
##     # All dates:
##     m = length(dst.dates)
##     # Put them in the right order:
##     # Added DW: 2005-05-27
##     idx = order(o[which(o>m)])
##     offSets = yout[idx]
##     dt = strptime(charvec, "%Y-%m-%d %H:%M:%S", tz = "GMT")
## 
##     ## Return Value:
##     format(dt + signum * offSets, format="%Y-%m-%d %H:%M:%S")
## }


strptimeDate <- function(x, format = whichFormat(x), tz = "") {
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates for character time stamps a 'timeDate' object

    # Example:
    #   timeDate(); strptimeDate(as.character(Sys.timeDate()))

    # Note:
    #   This function works like strptime.

    # FUNCTION:

    # Check Arguments:
    stopifnot(is.character(x))

    # Settings and Checks:
    if (tz == "")
        tz = getRmetricsOptions("myFinCenter")

    # Create 'timeDate':
    ans = timeDate(x, format, zone = tz, FinCenter = tz)

    # Return Value:
    ans
}

################################################################################
