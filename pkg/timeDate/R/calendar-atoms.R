
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
# METHOD:                   DESCRIPTION:
#  atoms,timeDate            Returns date/time atoms from a 'timeDate' object
#  months,timeDate           Extracts months atom from a 'timeDate' object
################################################################################
 
setMethod("atoms", "timeDate", function(x, ...)
{
    # A function implemented by Diethelm Wuertz
    # and improved by Yohan Chalabi

    # Description:
    #   Extracts atoms from a 'timeDate' object.

    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       calendar "atoms".

    # Value:
    #   Returns a data.frame with the following calendar atoms:
    #   Y(ear), m(onth), d(ay), H(our), M(inutes), S(econds).

    # FUNCTION:

    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")

    ## mdy:
    ##
    ## 2023-12-19 GNB:
    ##
    ##    if you wonder why this works, see my comments in .formatFinCenterNum(). Indeed, it
    ##    looks peculiar since the date at GMT may be different from the date at FinCenter!
    ##
    ##    To summarize, .formatFinCenterNum() computes (with "gmt2any") a GMT time which will
    ##    print the same as the time of x in FinCenter. as.POSIXlt() calls
    ##    .formatFinCenterNum() indirectly, so the same holds for it.
    ##
    ## TODO: due to the above, the timeDate methods for as.POSIXlt and as.POSIXct are
    ##       misleading, they do something somewhat different. Since this seems to be used at
    ##       a number of places in timeDate, it is not straightforward to fix this (assuming
    ##       I am correct). One way would be to create a new generic with a slightly
    ##       different name, maybe not exported, with methods the current methods for
    ##       as.POSIXlt and as.POSIXct, then rename the calls in the package to use that (for
    ##       example, the one below). When everything is sorted out the as.POSIXlt and
    ##       as.POSIXct methods can be fixed.
    
    X <- as.POSIXlt(x, tz = "GMT")
    Y <- X$year + 1900
    m <- X$mon + 1
    d <- X$mday
    H <- X$hour
    M <- X$min
    S <- X$sec

    # Data Frame:
    ans <- data.frame(Y = Y, m = m, d = d, H = H, M = M, S = S)
    attr(ans, "control") <- c(FinCenter = finCenter(x))

    # Return Value:
    ans
})


setMethod("atoms", "ANY", function(x, ...)
{
    # A function implemented by Diethelm WUertz

    # FUNCTION:

    # Return Value:
    invisible(x)
})


################################################################################


## 2023-12-19 GNB: 
## 
## TODO: the default base R months() gives a character vector of the names of
##       the months, (in the current locale, I think). This method returns
##       integers, which is inconsistent. *Fix this?*
##       (DONE: Fixed below. Still, eventually remove the numeric option?
##
## GNB: for now changing this method to be for missing 'abbreviate' and adding a
##      method for non-missing. So, the character names can be obtained by
##      setting it to TRUE or FALSE explicitly.
##
## GNB: further to the above, make the method S3, no need to make months() S4.
##      Also, the S4 method was not visible when timeDate was loaded but not
##      attached.
months.timeDate <- function(x, abbreviate = FALSE) {
    if(missing(abbreviate)) {
        ## Original definition: implemented by Diethelm Wuertz
        ##                      and improved by Yohan Chalabi
        if (!inherits(x, "timeDate"))
            stop("Wrong class type")
        ans <- as.POSIXlt(x, tz = "GMT")$mon + 1
        attr(ans, "control") <- c(FinCenter = finCenter(x))
        ans
    } else
        ## GNB
        format(x, if(abbreviate) "%b" else "%B")
}


################################################################################


## 2023-12-20
##    author: GNB:
##    methods for weekdays() and quarters()

weekdays.timeDate <- function(x, abbreviate = FALSE) {
    format(x, if(abbreviate) "%a" else "%A")
}

quarters.timeDate <- function(x, abbreviate) {
    X <- as.POSIXlt(x, tz = "GMT") 
    paste0("Q", X$mon %/% 3 + 1) # X$mon is in {0,1,...,11}
}

## 2023-12-19
##    author: GNB:
##    new function with methods for completion in IDEs

atom_names <-
    c("year", "month", "day", "hour", "minute", "second",
      "wday", "weekday", "wday0", "weekday0", "quarter")
    
if(getRversion() >= '4.3.0') {
    ## (GNB) see ?balancePOSIXlt in R > = 4.3.0; before that subscripting was
    ##     not guaranteed to have the expected length; If I understand correctly
    ##     the description there, from R 4.3.0 that is guaranteed.
    setMethod("$", "timeDate", function(x, name) {
        nam <- as.character(name)
        ## see GNB's notes for the method for months()
        X <- as.POSIXlt(x, tz = "GMT")
        
        res <- switch(nam,
                      year     = X$year + 1900,
                      month    = X$mon + 1 ,
                      day      = X$mday    ,
                      hour     = X$hour    ,
                      minute   = X$min     ,
                      second   = X$sec     ,
                      wday = ,
                      weekday  = {tmp <- X$wday; tmp[tmp == 0] <- 7; tmp},
                      wday0 = ,
                      weekday0 = X$wday, #  Sun - 0
                      quarter  = paste0("Q", X$mon %/% 3 + 1), # X$mon is in {0,1,...,11}
                      ## default
                      stop("unknown (or not implemented) time component requested")
                      )
        ## TODO: add names using format(x)?
        res
    })
} else {
    ## x$min etc may be shorter; convert to data.frame to ensure all have same length;
    ##
    ## TODO: need to test this case on R < 4.3.0
    setMethod("$", "timeDate", function(x, name) {
        nam <- as.character(name)
        ## see GNB's notes for the method for months()
        X <- data.frame(unclass(as.POSIXlt(x, tz = "GMT")))
        res <- switch(nam,
                      year     = X$year + 1900,
                      month    = X$mon + 1 ,
                      day      = X$mday    ,
                      hour     = X$hour    ,
                      minute   = X$min     ,
                      second   = X$sec     ,
                      weekday  = {tmp <- X$wday; tmp[tmp == 0] <- 7; tmp},
                      weekday0 = X$wday, #  Sun - 0
                      quarter  = paste0("Q", X$mon %/% 3 + 1), # X$mon is in {0,1,...,11}
                      ## default
                      stop("unknown (or not implemented) time component requested")
                      )
        ## TODO: add names using format(x)?
        res
    })
}    

## TODO: check when .DollarNames() was introduced.
## DONE: in R-2.10.0 (i.e. forever, since timeDate depends on R >= 3.6.0) 
##
##       a method could still be defined but with my findMatches.
##
if(getRversion() >= '4.3.0') { ## new function .AtNames(); also: findMatches() is now exported
    .DollarNames.timeDate <- function(x, pattern = "") {    
        utils::findMatches(pattern, atom_names)
    }
} else { ## simplified matches, .DollarNames available but findMatches not (or not exported)
    ## TODO: needs testing on R < 4.3.0
    .DollarNames.timeDate <- function(x, pattern = "") {    
        grep(pattern, atom_names, value = TRUE)
    }
}
