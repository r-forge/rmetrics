\name{merge}                     
\alias{merge}

\alias{merge,ANY,timeSeries-method}

\alias{merge,timeSeries,ANY-method}
\alias{merge,timeSeries,missing-method}
\alias{merge,timeSeries,numeric-method}
\alias{merge,timeSeries,matrix-method}
\alias{merge,timeSeries,timeSeries-method}

\alias{merge,matrix,timeSeries-method}
\alias{merge,numeric,timeSeries-method}


\title{Merges two 'timeSeries' objects}

\description{

    Merges several object types with \code{"timeSeries"} objects.
    The number of rows must match.
    
}

\usage{
merge(x, y, \dots)
}



\arguments{
  \item{x,y}{
    objects to merge, at least one of class \code{"timeSeries"}.
  }
  \item{...}{additional arguments for methods}
}

\details{

    The following combinations are supported:
    
    \tabular{ll}{
    \code{timeSeries} \tab ANY \cr
    \code{timeSeries} \tab missing \cr
    \code{timeSeries} \tab numeric \cr
    \code{timeSeries} \tab matrix \cr
    \code{timeSeries} \tab timeSeries }
        
}


\value{

  a \code{"timeSeries"} object
    
}

\examples{
## Load Series -
   x <- MSFT[1:12, ]

## Merge 'timeSeries' with missing Object -
   merge(x)

\dontshow{set.seed(1234)}
## Merge 'timeSeries' with numeric Object -
   y <- rnorm(12)
   class(y)
   merge(x, y)

## Merge 'timeSeries' with matrix Object -
   y <- matrix(rnorm(24), ncol=2)
   class(y)
   merge(x, y)

## Merge 'timeSeries' with matrix Object -
   y <- timeSeries(data=rnorm(12), charvec=time(x))
   class(y)
   merge(x, y)
}


\keyword{chron}
\keyword{ts}
 
