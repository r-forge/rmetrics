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

.make_disjoint <- function(left, right) {
    le <- unclass(left@Data)
    ri <- unclass(right@Data)

    N <- length(le)

    stopifnot(length(ri) == N)
    stopifnot(all(le < ri))  # TODO: cater for NAs

    if(length(le) <= 1)
        return(list(left, right))

    sind <- order(le)
    le <- le[sind]
    ri <- ri[sind]

    dj <- rep(TRUE, N)
    i <- 1
    for(i in 1:(N - 1)) {
        if(ri[i] >= le[i + 1]) {  # touching or overlapping
            le[i + 1] <- le[i]
            if(ri[i] > ri[i + 1]) # A_i contains A_{i+1}
                ri[i + 1] <- ri[i]
            dj[i] <- FALSE
        }
    }

    ## maybe not bother with tz?
    left@Data <- .POSIXct(le[dj], tz = attr(left@Data, "tz"))
    right@Data <- .POSIXct(ri[dj], tz = attr(right@Data, "tz"))

    ## print(left)
    ## print(right)

    list(left, right)
}


timeInterval <- function(left, right, ...) {
    l <- timeDate(left, ...)
    r <- timeDate(right, ...)

    new("timeInterval", left = l, right = r)
}

setGeneric("timeInterval")


setMethod("timeInterval", c("timeInterval", "missing"), function(left, right, ...) {
    new("timeInterval", left)
})

setMethod("timeInterval", c("timeDate", "timeDate"), function(left, right, ...) {
    right@format <- left@format
    new("timeInterval", left = left, right = right)
})


setMethod("timeInterval", c("timeDate", "missing"), function(left, right, units = "days", ...) {
    l <- trunc(left, units)
    r <- timeCeiling(left, units)
    if(any(flags <- l == r)) { # already rounded to 'units'
        r[flags] <- timeCeiling(r[flags] + 0.25, units)
    }
    new("timeInterval", left = l, right = r)
})

setMethod("timeInterval", c("missing", "timeDate"), function(left, right, units = "days", ...) {
    r <- trunc(right, units)
    l <- trunc(r - 0.25, units)
    new("timeInterval", left = l, right = r)
})


setMethod("timeInterval", c("Date", "missing"), function(left, right, units = "days", ...) {
    l <- timeDate(left, ...)
    r <- timeCeiling(l + 0.25, units)
    new("timeInterval", left = l, right = r)
})

setMethod("timeInterval", c("missing", "Date"), function(left, right, units = "days", ...) {
    r <- timeDate(right, ...)
    l <- trunc(r - 0.25, units)
    new("timeInterval", left = l, right = r)
})


setMethod("timeInterval", c("POSIXt", "missing"), function(left, right, units = "days", ...) {
    wrk <- timeDate(left, ...)
    l <- trunc(wrk, units)
    r <- timeCeiling(wrk, units)
    if(any(flags <- l == r)) { # already rounded to 'units'
        r[flags] <- timeCeiling(r[flags] + 0.25, units)
    }
    new("timeInterval", left = l, right = r)
})

setMethod("timeInterval", c("missing", "POSIXt"), function(left, right, units = "days", ...) {
    wrk <- timeDate(right, ...)
    r <- trunc(wrk, units)
    l <- trunc(r - 0.25, units)
    new("timeInterval", left = l, right = r)
})


## set operations

union.timeInterval <- function(x, y, ...) {
    stopifnot(is(y, "timeInterval"))

    ## TODO: more than 2 arguments
    x@left@Data <- c(x@left@Data, y@left@Data)
    x@right@Data <- c(x@right@Data, y@right@Data)

    timeInterval(x)

}

intersect.timeInterval <- function(x, y, ...) {
    stopifnot(is(y, "timeInterval"))

    ## TODO: more than 2 arguments
    x@left@Data <- c(x@left@Data, y@left@Data)
    x@right@Data <- c(x@right@Data, y@right@Data)

    timeInterval(x)

}

