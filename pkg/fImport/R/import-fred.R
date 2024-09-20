
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:             DESCRIPTION:
#  fredImport            Downloads market data from research.stlouisfed.org
#  fredSeries            Easy to use download from research.stlouisfed.org
################################################################################


fredImport <- function(query, file = "tempfile", source = NULL,
                       frequency = "daily",
                       from = NULL, to = Sys.timeDate(), nDaysBack = NULL,
                       save = FALSE, sep = ";", try = TRUE) {
    ## A function implemented by Diethelm Wuertz
    ##     Modified by GNB to work with html (using html2 and rvest).

    # Description:
    #   Downloads Monthly Market Data, Indices and Benchmarks from
    #   St. Louis FED, "research.stlouisfed.org".

    # Value:
    #   An One Column data frame with row names denoting the dates
    #   given in the POSIX format "%Y%m%d". The column lists the
    #   downloaded data records.

    # Examples:
    #   fredImport("DPRIME")

    # Notes:
    #   This function is written for one-column daily data sets.
    #   Some example data sets include:
    #     DEXUSEU   U.S. / Euro Foreign Exchange Rate
    #     DEXSZUS   Switzerland / U.S. Foreign Exchange Rate
    #     DGS1      1-Year Treasury Constant Maturity Rate
    #     DPRIME    Bank Prime Loan Rate

    # FUNCTION:

    # Settings:
    stopifnot(length(query) == 1)

    # Source"
    if (is.null(source))
        ## 2024-09-19 was:  source = "http://research.stlouisfed.org/fred2/series/"
        source <- "https://fred.stlouisfed.org"

    # Check:
    if (frequency != "daily")
        stop("Only daily data records are supported!")

    # Download:
    if (try) {
        ## Try for Internet Connection:
        z = try(fredImport(query, file, source, frequency, from, to,
                           nDaysBack, save, sep, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access or another error")
        } else {
            return(z)
        }
    } else {
        ## Download File:
        ## was:
        ##     queryFile = paste(query, "/downloaddata/", query, ".txt", sep = "")
        ##     url = paste(source, queryFile, sep = "")
        queryFile <- paste0("/data/", query)
        url <- paste0(source, queryFile)

        tmp <- tempfile()
        download.file(url = url, destfile = tmp)

        ## # Scan the file:
        ## was:
        ##     x1 = scan(tmp, what = "", sep = "\n")
        ##
        ##     # Extract dates ^19XX and ^20XX:
        ##     x2 = x1[regexpr(pattern="^[12][90]", x1, perl=TRUE) > 0]
        ##     x1 = x2[regexpr(pattern=" .$", x2, perl=TRUE) < 0]
        ##
        ##     # Compose Time Series:
        ##     data <- matrix(
        ##       as.numeric(substring(x1, 11, 999)), byrow = TRUE, ncol = 1)
        ##     charvec <- substring(x1, 1, 10)
        if(!requireNamespace("xml2") || !requireNamespace("rvest"))
            stop("this function requires packages 'xml2' and 'rvest',please install them")
        xml2 <- xml2::read_html(tmp, encoding = "UTF-8")

        tbls <- rvest::html_table(xml2)
        datatbl <- tbls[[2]]

        ## VALUE is character since '.' stands for NA;
        ## TODO (GNB):
        ##      maybe should be defensive here - what happens if there are no missing values?
        ##      will VALUE be still character?
        datatbl$VALUE[datatbl$VALUE == "."] <- NA
        datatbl$VALUE <- as.numeric(datatbl$VALUE)

        data <- matrix(datatbl$VALUE, ncol = 1)
        charvec <- datatbl$DATE

        X <- timeSeries(data, charvec, units = query)

      # Time Window:
      if (is.null(to)) to <- Sys.timeDate()
      to <-  trunc(as.timeDate(to),"days")

      if (is.null(from)) {
        if (is.null(nDaysBack)) {
          from <- start(X)
        } else {
          from <- to - nDaysBack*24*3600
        }
      }
      from <- trunc(as.timeDate(from), "days")

      X <- window(X, from, to)
    }

    # Save to file:
    if (save) {
      write.table(as.data.frame(X), file = file, sep = sep)
    } else {
      unlink(file)
    }

    # Result:
    ## TODO (GNB): could do better - tbls[[1]] contains header information
    ans <- new("fWEBDATA",
               call = match.call(),
               param = c(
                 "Instrument" = query,
                 "Frequency " = frequency),
               data = X,
               title = "Data Import from research.stlouisfed.org",
               description = description() )

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------
fredSeries <-
  function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, ...)
  {
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads easily time series data from St. Louis FRED

    # Arguments:
    #   symbols - a character vector of symbol names
    #   from - from date
    #   to - to date
    #   nDaysBack - number of n-days back
    #   ... - arguments passed to the *Import()

    # Examples:
    #   fredSeries("DPRIME")[1:10, ]

    # FUNCTION:

    # Download:
    X <- fredImport(query = symbols[1],
                    from = from, to = to, nDaysBack=nDaysBack, ...)@data
    N <- length(symbols)
    if (N > 1) {
      for (i in 2:N) {
        X <- merge(X, fredImport(query = symbols[i],
                                 from = from, to = to, nDaysBack=nDaysBack, ...)@data)
      }
    }

    # Return Value:
    X
  }


################################################################################


