
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  skewness                  Returns a number which is the skewness of the data
#  skewness.default          Default method
#  skewness.data.frame       Method for objects of class data.frame
#  skewness.POSIXct          Method for objects of class POSIXct
#  skewness.POSIXlt          Method for objects of class POSIXlt
################################################################################


skewness <-
    function (x, ...)
{
    # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    UseMethod("skewness")
}


# ------------------------------------------------------------------------------


skewness.default <-
    function (x, na.rm = FALSE, method = c("moment", "fisher"), ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the value of the skewness of a distribution function.

    # Details:
    #   Missing values can be handled.

    # FUNCTION:

    # Method:
    method = match.arg(method)

    # Warnings:
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))}

    stopifnot(NCOL(x) == 1)

    # Remove NAs:
    if (na.rm) x = x[!is.na(x)]

    # Skewness:
    n = length(x)
    if (is.integer(x)) x = as.numeric(x)

    # Selected Method:
    if (method == "moment") {
        skewness = sum((x-mean(x))^3/sqrt(as.numeric(var(x)))^3)/length(x)
    }
    if (method == "fisher") {
        if (n < 3)
            skewness = NA
        else
            skewness = ((sqrt(n*(n-1))/(n-2))*(sum(x^3)/n))/((sum(x^2)/n)^(3/2))
    }

    # Add Control Attribute:
    attr(skewness, "method") <- method

    # Return Value:
    skewness
}


# ------------------------------------------------------------------------------


skewness.data.frame <-
    function (x, ...)
{
    # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    sapply(x, skewness, ...)
}


# ------------------------------------------------------------------------------


skewness.POSIXct <-
    function (x, ...)
{
    # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    structure(skewness(unclass(x), ...), class = c("POSIXt", "POSIXct"))
}


# ------------------------------------------------------------------------------


skewness.POSIXlt <-
    function (x, ...)
{
    # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Value:
    as.POSIXlt(skewness(as.POSIXct(x), ...))
}


################################################################################
