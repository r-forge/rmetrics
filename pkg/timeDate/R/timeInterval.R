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

.intersect.timeInterval <- function(e1, e2) {
    ti1 <- e1
    ti2 <- e2

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

#print("Ah")
    if(Nl == 0 || Nr == 0) # empty intersection
        return(ti1)
    else if (Nl == 1) { # Nr is also 1 here
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
#print("Oh")
    ## from here: Nl >= 2, Nr >= 1, Nl >= Nr

    ## lr1 and lr2 are strictly increasing (if make_disjoint is correct) !!!
    lr1 <- as.vector(rbind(ti1_le, ti1_ri)) # a1, b1, a2, b2, ...
    lr2 <- as.vector(rbind(ti2_le, ti2_ri))

    i <- findInterval(lr2, lr1)
    i_even <- !(i %% 2)

    res_flag <- rep(FALSE, length(lr1))

    pieces <- numeric(0)
    j <- 1
    while(j < 2 * Nr) {
        ij <- i[j]
        ij1 <- i[j + 1]
        ## if(ij == 0) {
        ##     if(ij1 == 0) {
        ##         j <- j + 2
        ##         next
        ##     } else if(ij1 == 1) {
        ##         pieces <- c(pieces, c(lr1[1], lr2[j + 1]))
        ##         j <- j + 2
        ##         next
        ##     }
        ##     ???
        ## }
        dj <- ij1 - ij
        if(i_even[j]) {# between the end of an interval and the start of the next one
            if(dj == 0) {
                ## nothing to do
            } else if(ij1 %% 2 == 0) {
                ## whole intervals between ij + 1 and i[j + 1]
                res_flag[(ij + 1) : ij1] <- TRUE
            } else {
                ## i[j+1] is inside an lr2 interval, so the last interval is not whole
                if(ij + 1 < ij1 - 1)
                    res_flag[(ij + 1) : (ij1 - 1)] <- TRUE ## the whole intervals
                if(lr1[ij1] < lr2[j + 1]) # i.e. not equal
                    pieces <- c(pieces, c(lr1[ij1], lr2[j + 1])) # the last piece; its rhs
                                                                 # is replaced by the rhs of
                                                                 #  the current rhs
            }
        } else { # ij odd; inside of an interval
            if(dj == 0) {
                ## wholly inside an lr1 interval
                pieces <- c(pieces, c(lr2[j], lr2[j + 1]))
            } else {
                pieces <- c(pieces, c(lr2[j], lr1[ij + 1])) # notice the rhs of the interval
                if(ij1 %% 2 == 0) {
                    ## whole intervals  between ij + 1 (or ij) and i[j + 1];
                    ## the first starts with lr2[j] and is not whole, unless lr2[j] == lr1[ij]
                    if(ij + 2 < ij1)
                        res_flag[(ij + 2) : ij1] <- TRUE
                } else {
                    ## i[j+1] is inside an interval, so the last interval is not whole
                    if(ij + 1 < ij1 - 1)
                        res_flag[(ij + 1) : (ij1 - 1)] <- TRUE ## the whole intervals
                    if(lr1[ij1] < lr2[j + 1]) # in case they are equal
                        pieces <- c(pieces, c(lr1[ij1], lr2[j + 1])) # the last piece; its rhs is
                                                                 # replaced by the rhs of the
                                                                 # current
                }
            }
        }
#browser()

        j <- j + 2
    }
#browser()

    wrk <- matrix(c(lr1[res_flag], pieces), ncol = 2, byrow = TRUE)

    ti1@left@Data <- .POSIXct(wrk[ , 1], tz = attr(ti1@left@Data, "tz"))
    ti1@right@Data <- .POSIXct(wrk[ , 2], tz = attr(ti1@right@Data, "tz"))

    timeInterval(ti1)
}

.setdiff_timeInterval <- function(e1, e2) {
    ## TODO: eventually implement it directly, similarly to intersection
    e2 & !e1
}

setMethod("&", c("timeInterval", "timeInterval"), .intersect.timeInterval)
setMethod("|", c("timeInterval", "timeInterval"),
          function(e1, e2) .union.timeInterval(e1, e2) )
setMethod("^", c("timeInterval", "timeInterval"), .setdiff_timeInterval)


################################################################################

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

################################################################################

## set operations

`!.timeInterval` <- function(x) {
    wrk <- as.vector(rbind(x@left@Data, x@right@Data))

    wrk <- if(wrk[1] != -Inf)
               c(-Inf, wrk)
           else
               wrk[-1]

    k <- length(wrk)
    wrk <- if(wrk[k] == Inf)
               wrk[-k]
           else
               c(wrk, Inf)

    wrk <- matrix(wrk, ncol = 2, byrow = TRUE)

    x@left@Data <- .POSIXct(wrk[ , 1], tz = attr(x@left@Data, "tz"))
    x@right@Data <- .POSIXct(wrk[ , 2], tz = attr(x@right@Data, "tz"))

    ## was: timeInterval(x) -  but x already sorted, so just return it
    x
}

.union.timeInterval <- function(x, y, ...) {
    stopifnot(is(y, "timeInterval"))

    ## TODO: more than 2 arguments
    x@left@Data <- c(x@left@Data, y@left@Data)
    x@right@Data <- c(x@right@Data, y@right@Data)

    timeInterval(x)

}


## %in_int%

## x - timeInterval
.int_in_int <- function(x, ti) {
    wrk1 <- as.vector(rbind(x@left@Data, x@right@Data))
    wrk2 <- as.vector(rbind(ti@left@Data, ti@right@Data))

    n1 <- length(wrk1)
    n2 <- length(wrk2)

    if(wrk1[1] < wrk2[1] || wrk1[n1] > wrk2[n2])
        return(FALSE)

    i <- findInterval(wrk1, wrk2)

    imat <- matrix(i, ncol = 2, byrow = TRUE)

    all(imat[ , 1] %% 2 != 0) && all(imat[ , 2] - imat[ , 1] == 1)
}

## x - timeDate
.td_in_int <- function(x, ti) {
    wrk1 <- as.vector(x@Data)
    wrk2 <- as.vector(rbind(ti@left@Data, ti@right@Data))

    n2 <- length(wrk2)

    i <- findInterval(wrk1, wrk2)

    i %% 2 != 0
}

`%in_int%` <- function(x, ti) {
    stop("No suitable method found for %in_int% for signature c(",
         class(x), ",", class(ti), ").\n")
}

setGeneric("%in_int%")

setMethod("%in_int%", c("timeInterval", "timeInterval"), .int_in_int)
setMethod("%in_int%", c("timeDate", "timeInterval"), .td_in_int)



## should be after the methods for timeInterval
tiInf <- timeInterval(left = -Inf, right = Inf)

