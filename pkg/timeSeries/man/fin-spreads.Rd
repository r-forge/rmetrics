\name{spreads}
\alias{spreads}
\alias{midquotes}

\alias{spreadSeries}
\alias{midquoteSeries}


\title{Spreads and mid quotes}

\description{
    
  Compute spreads and midquotes from price streams.
       
}

\usage{  
spreads(x, which = c("Bid", "Ask"), tickSize = NULL)    
midquotes(x, which = c("Bid", "Ask"))

midquoteSeries(\dots)
spreadSeries(\dots)
}

\arguments{

  \item{x}{
    an object of class \code{timeSeries}.
  }
  \item{which}{

    a vector with two character strings naming the column names of the
    time series from which to compute the mid quotes and spreads.  By
    default these are the bid and ask prices with column names
    \code{c("Bid", "Ask")}.

  }
  \item{tickSize}{

    the default is \code{NULL} to simply compute price changes in
    original price levels. If \code{ticksize} is supplied, the price
    changes will be divided by the value of \code{inTicksOfSize} to
    compute price changes in ticks.

  }         
  \item{\dots}{
    arguments to be passed.
  }
}

\value{
  all functions return an object of class \code{timeSeries}
}

\note{

  The functions \code{midquoteSeries} and \code{spreadSeries} may be
  deprecated and removed. They are synonyms for \code{midquotes} and
  \code{spreads}, respectively. Please use the shorter names.

}

\seealso{
  \code{\link{returns}},
  \code{\link{cumulated}},
  \code{\link{drawdowns}},
  \code{\link{splits}},
  %\code{\link{spreads}},
  \code{\link{midquotes}},
  \code{\link{index2wealth}}
}

\examples{
## Load the Microsoft Data -  
   setRmetricsOptions(myFinCenter = "GMT")
   data(MSFT)
   X = MSFT[1:10, ]
   head(X)

## Compute Open/Close Midquotes -
   X.MID <- midquotes(X, which = c("Close", "Open"))
   colnames(X.MID) <- "X.MID"
   X.MID

## Compute Open/Close Spreads -
   X.SPREAD <- spreads(X, which = c("Close", "Open"))
   colnames(X.SPREAD) <- "X.SPREAD"
   X.SPREAD
}

\keyword{chron}
