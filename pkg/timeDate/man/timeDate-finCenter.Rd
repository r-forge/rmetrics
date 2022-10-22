\name{finCenter}
\alias{finCenter}

\alias{finCenter,timeDate-method}

\alias{finCenter<-}
\alias{finCenter<-,timeDate-method}

\title{Financial Center of a timeDate object}

\description{
  Get or set the financial center of a \code{"timeDate"} object.
}

\usage{
\S4method{finCenter}{timeDate}(x)
\S4method{finCenter}{timeDate}(x) <- value
}


\arguments{
  \item{x}{
    a \code{timeSeries} object.
  }
  \item{value}{
    a character with the location of the  financial center named 
    as "continent/city".
  }
}

\details{

  \code{"timeDate"} objects store the time in the GMT time zone. The
  financial center specifies a location whose local time is to be used
  to format the object, e.g., for printing.
  
  \code{finCenter} gives the financial center associated with a
  \sQuote{timeDate} object. The assignment form changes it to the
  specified value.  Both functions are S4 generics. This page describes
  the methods defined in package \sQuote{timeDate}.

}

\seealso{
  \code{\link{listFinCenter}}
}

\examples{
date <- timeDate("2008-01-01")
finCenter(date) <- "GMT"
date
format(date)

finCenter(date) <- "Zurich"
date
format(date)
}
