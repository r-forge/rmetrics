\name{holidayNYSE}
\alias{holidayNYSE}


\title{NYSE holiday calendar}

\description{
    
    Returns a holiday (closing days) calendar for the New York Stock
    Exchange.
                
}

\usage{
holidayNYSE(year = getRmetricsOptions("currentYear"), 
            type = c("", "standard", "special"))
}

\details{

  \code{holidayNYSE} generates a list of the clossing days of the
  exchange for the requested years.

  The default is to return all closing days (holidays and
  specials). \code{type = "standard"} requests only closings associated
  with the standard public holidays, \code{type = "special"} gives the
  special closings only.

}

\arguments{

  \item{year}{
    an integer value or vector of years, formatted as \code{YYYY}.
  }
        
  \item{type}{

    what to include, a character string. The default is to return all
    closing days (holidays and specials). \code{"standard"} requests
    only closings associated with the standard public holidays,
    \code{"special"} gives the special closings only. 
    
  }

}

\note{

  The list of closing days returned by \code{holidayNYSE} was changed in
  \pkg{timeDate} version 4021.105, in that previously it did not include
  special closing days. This was perceived by some users as buggy. Also,
  the intent by the authors of the package seems to have been for it to
  return all closing days. Indeed, the default for \code{isisBizday()}
  is to drop weekends and days returned by \code{holidayNYSE}.

  Argument \code{type} was also included in version 4021.105. The old
  behaviour can be obtained by using \code{type = "standard"}.

  The default for argument \code{type} is currently the empty string,
  since I couldn't come up with another string that would be universally
  easy to remember. Suggestions are welcome but a change will be only
  feasible if they come soon. 

}

\author{
  Diethelm Wuertz (original author);
  Yohan Chalabi improved speed and handling of time zone;
  Georgi N. Boshnakov added the special closings and argument
  \sQuote{type}. 
}

\value{
    an object of class \code{"timeDate"}
}


\examples{
## holidayNYSE -
holidayNYSE() ## current year
holidayNYSE(2008:2010)

## January 2, 2007 was a memorial day for president G.R. Ford,
##    not a regular public holiday
holidayNYSE(2007)
holidayNYSE(2007, type = "standard")
holidayNYSE(2007, type = "special")
}

\keyword{chron}
