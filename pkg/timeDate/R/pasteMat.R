## paste the columns of a matrix or df, Any row containing one or more NAs
## results in an NA in the corresponding element of the result.
##
## dts <- c("1989-09-28", NA, "2004-08-30", "1990-02-09")
## tms <- c(  "23:12:55",   "10:34:02",   NA,   "11:18:23")
## ## this throws error (since NAs are converted to the string NA):
## timeDate(paste(dts,tms), FinCenter = "Europe/Zurich")
## ## ## Error in midnightStandard2(charvec, format) :
## ## ##   'charvec' has non-NA entries of different number of characters
##
## ## this works:
## td1 <- timeDate(pasteMat(cbind(dts, tms)), FinCenter = "Europe/Zurich")
## td2 <- timeDate(pasteMat(dts, tms), FinCenter = "Europe/Zurich")
## identical(td1, td2)
##
## identical(pasteMat(dts, tms), pasteMat(cbind(dts, tms)))
## ## TRUE
##
## 'sep' can be a vector, allowing to use different separators between different columns
pasteMat <- function(x, ...,  sep = NULL){
    ## Author: GNB

    if(...length() > 0)
        x <- cbind(x, ...)

    wrk <- asplit(x, 2) # x matrix or df; asplit is available since R-3.6.0
    na_ind <- apply(x, 2, function(z) which(is.na(z)))
    na_ind <- unique(unlist(na_ind))

    if(is.null(sep)) {
        res <- do.call(paste, wrk)
    } else {
        nc <- length(wrk)
        ind <- as.vector(matrix(1:(2 * nc), nrow = 2, byrow = TRUE))[-2 * nc]
        if(length(sep) != nc - 1)
            sep <- rep(sep, length.out = nc - 1)
        res <- c(wrk, sep)[ind]
        res <- do.call(paste0, res)
    }
    res[na_ind] <- NA_character_

    res
}
