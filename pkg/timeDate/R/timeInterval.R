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

    if(N <= 1)
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


intersect.timeInterval <- function(ti1, ti2) {
    stopifnot(is(ti2, "timeInterval"))

    Nl <- length(ti1@left@Data)
    Nr <- length(ti2@left@Data)

    ## swap to reduce branching below; at the end take tzone, format from original 'ti1'
    ## swap the data only to keep same FinCenter (but format?)
    ##
    ## :TODO: for now just swap, fix the FinCenter later
    if(Nl < Nr) {
        tmp <-ti1
        ti1 <- ti2
        ti2 <- tmp

        tmp <- Nl
        Nl <- Nr
        Nr <- tmp
    }
    ## guaranteed Nl >= Nr from here

    ti1_le <- unclass(ti1@left@Data)
    ti1_ri <- unclass(ti1@right@Data)

    ti2_le <- unclass(ti2@left@Data)
    ti2_ri <- unclass(ti2@right@Data)

    ## .POSIXct(numeric(0), tz = "GMT") # TODO: timeInterval for empty?

    if(Nl == 0 || Nr == 0) # empty intersection
        return(ti1)
    else if (Nr == 1) { # Nl is also 1 here
        ## remember that the intervals are semi-open: [a,b)
        lo <- max(ti1_le[1], ti2_le[1])
        up <- min(ti1_ri[1], ti2_ri[1])

        if(lo >= up) { # empty
            ti1@left@Data[1] <-
            ti1@right@Data[1] <- numeric(0)
        } else {
            ti1@left@Data[1] <- lo
            ti1@right@Data[1] <- up
        }
        return(ti1)
    }

    ## from here: Nl >= 2, Nr >= 1, Nl >= Nr

    ## lr1 and lr2 are strinctly increasing (if make_disjoint is correct) !!!
    lr1 <- as.vector(rbind(ti1_le, ti1_re)) # a1, b1, a2, b2, ...
    lr2 <- as.vector(rbind(ti2_le, ti2_re))

    i <- findInterval(lr2, lr1)
    i_even <- !(i %% 2)

    res_flag <- rep(FALSE, length(lr1))

    pieces <- numeric(0)
    j <- 1
    while(j < 2 * Nl) {
        ij <- i[j]
        ij1 <- i[j + 1]
        dj <- ij1 - ij
        if(i_even[ij]) {# between the end of an interval and the start of the next one
            if(dj == 0) {
                ## nothing to do
            } else if(i_even[ij1]) {
                ## whole intervals between ij + 1 and i[j + 1]
                res_flag[(ij + 1) : ij1] <- TRUE
            } else {
                ## i[j+1] is inside an interval, so the last interval is not whole
                res_flag[(ij + 1) : (ij1 - 1)] <- TRUE ## the whole intervals
                pieces <- c(pieces, c(ij1, lr1[j + 1])) # the last piece; its rhs is replaced
                                                        # by the rhs of the current
            }
        } else { # ij odd; inside of an interval
            if(dj == 0) {
                pieces <- c(pieces, c(lr1[j], lr1[j + 1]))
            } else if(i_even[ij1]) {
                ## whole intervals between ij + 1 and i[j + 1]
                res_flag[(ij + 1) : ij1] <- TRUE
            } else {
                ## i[j+1] is inside an interval, so the last interval is not whole
                res_flag[(ij + 1) : (ij1 - 1)] <- TRUE ## the whole intervals
                pieces <- c(pieces, c(lr2[ij1], lr1[j + 1])) # the last piece; its rhs is
                                                             # replaced by the rhs of the
                                                             # current
            }
        }

        j <- j + 2
    }

    ## maybe not bother with tz?
    ti1@Data <- .POSIXct(le[dj], tz = attr(ti1@Data, "tz"))
    ti2@Data <- .POSIXct(ri[dj], tz = attr(ti2@Data, "tz"))

    ## print(ti1)
    ## print(ti2)

    list(ti1, ti2)
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

