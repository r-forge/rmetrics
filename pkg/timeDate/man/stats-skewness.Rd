\name{skewness}
\alias{skewness}

\alias{skewness.default}
\alias{skewness.data.frame}
\alias{skewness.POSIXct}
\alias{skewness.POSIXlt}


\title{Skewness}

\description{
  Functions to compute skewness.
}

\usage{
skewness(x, \dots)

\method{skewness}{default}(x, na.rm = FALSE, method = c("moment", "fisher"), \dots)

\method{skewness}{data.frame}(x, na.rm = FALSE, method = c("moment", "fisher"), \dots)

\method{skewness}{POSIXct}(x, \dots)

\method{skewness}{POSIXlt}(x, \dots)
}

\arguments{
  
  \item{x}{
    a numeric vector or object.      
  }
  \item{na.rm}{
    a logical. Should missing values be removed?
  }
  \item{method}{
    
    a character string, the method of computation, see section
    \sQuote{Detaials}.
    
  }
  \item{\dots}{
    arguments to be passed.
  }
}

\details{

  Argument \code{method} can be one of \code{"moment"} or
  \code{"fisher"}.  The \code{"moment"} method is based on the
  definitions of skewness for distributions and this should be used when
  resampling (bootstrap or jackknife). The \code{"fisher"} method
  correspond to the usual "unbiased" definition of sample variance,
  although in the case of skewness exact unbiasedness is not possible.

  The data frame method computes the skewness of each column. 

}

\value{
    
  a numeric value or vector with attribute \code{"method"} indicating
  the method. For the data frame method the values are named using the
  columns names.
    
}

\seealso{
  \code{\link{kurtosis}}
}

\examples{   
r = rnorm(100)
mean(r)
var(r)
   
skewness(r)  
}

\keyword{univar}
