
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# METHOD:                 EXTRACTORS:
#  fitted.fGARCH           S3 fitted values for an object of class 'fGARCH'
################################################################################


setMethod(f = "fitted", signature(object = "fGARCH"), definition =
    function(object)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Fitted values method for an object of class fGARCH

    # Arguments:
    #   object - an object of class fGarch as returned by the function
    #       garchFit

    # FUNCTION:

    # Get numeric vector of fitted, optionally standardized
    fitted = object@fitted

    # Get original time series class:
    ans = slot(object, "data")
    
    ## GNB: temporary fix for #6789 (the data part of ans was not being replaced with
    ##    'fitted', hence the data was returned, rather than fitted)
    ##
    ## TODO: fix properly the code and the documentation.
    ##    Note: check also if f@data is set with the original object.
    ##          It seems that it is numeric
    if(is(object, "timeSeries")){
        Name = as.character(object@formula[2])
        attr(ans, "Name") <- Name
        ans@.Data <- if(is.matrix(fitted)) fitted else matrix(fitted, ncol = 1)
    } else if(inherits(ans, "ts") || is.numeric(ans)) {
        ans[] <- fitted
    } else {
        message(paste0("conversion to class '", class(ans), "' not supported yet,\n",
                       "returning slot fitted asis."))
        ans <- fitted
    }
    
    # Return Value:
    ans
})


################################################################################

