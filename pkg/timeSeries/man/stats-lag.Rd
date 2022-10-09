\name{lag}
\alias{lag}

\alias{lag,timeSeries-method}
\alias{lag.timeSeries}

\title{Lag a time series}


\description{
    
    Compute a lagged version of a 'timeSeries' object.        
   
}


\usage{
\S4method{lag}{timeSeries}(x, k = 1, trim = FALSE, units = NULL, \dots)
}


\arguments{
         
  \item{x}{
    an object of class \code{timeSeries}.
  } 
  \item{k}{   
    an integer value, the number of lags (in units of observations).
    By default 1.
  }        
  \item{trim}{
    a logical value. By default \code{TRUE}, the first missing
    observation in the return series will be removed.
  }
  \item{units}{

    an optional character string, which allows to overwrite the current
    column names of a \code{"timeSeries"} object. By default \code{NULL}
    which means that the column names are selected automatically.

  }
  \item{\dots}{
    arguments passed to other methods.
  }
}


\value{

  the lagged object of class 'timeSeries'.
        
}


\examples{
## Load Micsrosoft Data Set -
   x = MSFT[1:20, "Open"]
   
## Lag the timeSeries Object:
   lag(x, k = -1:1)
}


\keyword{chron}

