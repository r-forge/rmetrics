
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
# FUNCTION:                 DESCRIPTION:
#  show.timeDate             Prints 'timeDate' object
################################################################################

setMethod("show", "timeDate", function (object)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    ## when creating empty new("timeDate")
    ## 2023-12-15 GNB: was
    ##    return(str(object))
    ## but that is confusing (also the return value is not NULL as below)
    if (!length(slot(object, "Data"))) {
        cat(finCenter(object), "\n", sep = "")
        cat(class(object), "of length 0", "\n")
        return(invisible(NULL))
    }
    # Check records to get printed:
    maxRmetrics <- as.numeric(getRmetricsOptions("max.print"))
    maxR <- as.numeric(getOption("max.print"))
    max <- min(na.omit(c(maxRmetrics, maxR, Inf)))
    #-> Inf to cast case when maxRmetrics and maxR are NULL

    if (ptest <- ((omitted <- length(object) - max) > 0))
        object <- object[seq.int(max)]

    output <- format(object)
    layout <- paste0("[", output, "]")
    names(layout) <- names(output)

    # Print Results:
    cat(finCenter(object), "\n", sep = "")
    print(layout, quote = FALSE)

    # print message
    if (ptest)
        cat(gettextf("...\n [ reached getRmetricsOption('max.print') | getOption('max.print') -- omitted %i rows ]]\n", omitted))

    # Return Value:
    invisible(NULL) # 'show' returns an invisible 'NULL'. (cf. ?show)
})

setMethod("show", "timeInterval", function (object)
{
    ## A function implemented by Georgi N. Boshnakov
    ##            (modelled after show.timeDate)

    if ((len <- length(object@left)) == 0) {
        cat(finCenter(object@left), "\n", sep = "")
        cat("An empty", class(object), "\n")
        return(invisible(NULL))
    }
    # Check records to get printed:
    maxRmetrics <- as.numeric(getRmetricsOptions("max.print"))
    maxR <- as.numeric(getOption("max.print"))
    max <- min(na.omit(c(maxRmetrics, maxR, Inf)))
    #-> Inf to cast case when maxRmetrics and maxR are NULL

    if (ptest <- ((omitted <- length(object@left) - max) > 0)){
        object@left <- object@left[1:max]
        object@right <- object@right[1:max]
    }

    output <- format(object)
    layout <- output
    if(!is.null(names(output)))
        layout <- paste0(names(output), "\t", layout)

    ## Print Results:
    if(len > 1)
        cat("union of", len, "time intervals\n")
    else
        cat("simple time interval\n")

    cat(finCenter(object@left), "\n", sep = "")
    cat(layout, sep = "\n")

    # print message
    if (ptest)
        cat(gettextf("...\n [ reached getRmetricsOption('max.print') | getOption('max.print') -- omitted %i rows ]]\n", omitted))

    # Return Value:
    invisible(NULL) # 'show' returns an invisible 'NULL'. (cf. ?show)
})

################################################################################
