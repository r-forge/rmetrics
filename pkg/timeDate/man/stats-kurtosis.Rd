\name{kurtosis}
\alias{kurtosis}

\alias{kurtosis.default}
\alias{kurtosis.data.frame}
\alias{kurtosis.POSIXct}
\alias{kurtosis.POSIXlt}


\title{Kurtosis}

\description{

  Generic function for computation of kurtosis.
  The methods defined in package \pkg{timeDate} are described here.

}


\usage{
kurtosis(x, \dots)

\method{kurtosis}{default}(x, na.rm = FALSE,
                  method = c("excess", "moment", "fisher"), \dots)

\method{kurtosis}{data.frame}(x, na.rm = FALSE,
                  method = c("excess", "moment", "fisher"), \dots)

\method{kurtosis}{POSIXct}(x, \dots)

\method{kurtosis}{POSIXlt}(x, \dots)
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
    \sQuote{Details}.

  }
  \item{\dots}{
    arguments to be passed.
  }
}

\details{

  \code{kurtosis} is an S3 generic function.  This page describes the
  methods defined in package \code{dateTime}.

  Argument \code{"method"} can be one of \code{"moment"},
  \code{"fisher"}, or \code{"excess"}.  If \code{"excess"} is selected,
  then the value of the kurtosis is computed by the \code{"moment"}
  method and a value of 3 will be subtracted.  The \code{"moment"}
  method is based on the definitions of kurtosis for distributions and
  this method should be used when resampling (bootstrap or
  jackknife). The \code{"fisher"} method correspond to the usual
  "unbiased" definition of sample variance, although in the case of
  kurtosis exact unbiasedness is not possible.
  
  If \code{x} is numeric the kurtosis is computed according to the
  description given for argument \code{method}. A logical vector is
  treated as a vector of 1's and 0's.

  The \code{data.frame} method applies \code{kurtosis} recursively to
  each column. The \code{POSIXlt} method computes the kurtosis of the
  underlying numerical representation of the date/times. The method for
  \code{POSIXct} does the same after converting the argument to
  \code{POSIXlt}.

  The default method returns \code{NA}, with a warning, if it can't
  handle argument \code{x}.

}

\value{
    
    a numeric value or vector with attribute \code{"method"} indicating
    the method. 
    
}


\seealso{
    \code{\link{skewness}}
}

\examples{   
## mean -
## var -
   # Mean, Variance:
   r = rnorm(100)
   mean(r)
   var(r)
   
## kurtosis - 
   kurtosis(r)

   kurtosis(data.frame(r = r, r2 = r^2))

}

\keyword{univar}
