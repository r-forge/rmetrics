\name{rank}
\alias{rank}

\alias{rank,timeSeries-method}


\title{Sample ranks of a time series}

\description{
    
  Compute the sample ranks of the values of a 'timeSeries' object.
      
}

\usage{
\S4method{rank}{timeSeries}(x, na.last = TRUE, ties.method = )
}

\arguments{
        
  \item{x}{
    an univariate object of class \code{timeSeries}.
  }
  \item{na.last}{

    for controlling the treatment of \code{NA}s. If \code{TRUE}, missing
    values in the data are put last; if \code{FALSE}, they are put
    first; if \code{NA}, they are removed; if \code{"keep"} they are
    kept with rank \code{NA}.

  }
  \item{ties.method}{

    a character string specifying how ties are treated; can be
    abbreviated.

  }  
}

\details{

  If all components are different (and no \code{NA}s), the ranks are
  well defined, with values in \code{seq_len(x)}. With some values equal
  (called \sQuote{ties}), argument \code{ties.method} determines the
  result at the corresponding indices. The \code{"first"} method results
  a permutation with increasing values at each index set of ties. The
  \code{"random"} method puts these in random order, whereas the
  default, \code{"average"}, replaces them by their mean, and
  \code{"max"} and \code{"min"} replace them with their maximum and
  minimum respectively, the latter being the typical sports ranking.

  \code{NA} values are never considered to be equal: for \code{na.last =
  TRUE} and \code{na.last = FALSE} they are given distinct ranks in the
  order in which they occur in \code{x}.

}

\value{
  a \code{"timeSeries"} object
}

\examples{
## Load Microsoft Data -
   X <- 100 * returns(MSFT)

## Compute the Ranks -
   head(rank(X[, "Open"]), 10)
   
## Only Interested in the Vector, then use -
   head(rank(series(X[, "Open"])), 10)
}

\keyword{chron}
