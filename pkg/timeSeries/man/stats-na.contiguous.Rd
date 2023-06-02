\name{na.contiguous}

\alias{na.contiguous,timeSeries-method}

\alias{is.na}
\alias{is.na,timeSeries-method}


\title{Find longest contiguous stretch of non-NAs or check for NAs}

\description{

  Find the longest consecutive stretch of non-missing values in a
  \code{"timeSeries"} object. In the event of a tie, the first such
  stretch. Also, \code{"timeSeries"} method for \code{is.na}.
      
}

\usage{
\S4method{na.contiguous}{timeSeries}(object, ...)
}

\arguments{
  \item{object}{
    a \code{"timeSeries"} object.
  }
  \item{\dots}{
    further arguments passed to other methods.
  }  
}

\value{

  for the \code{na.contiguous} method, a \code{"timeSeries"} object
  without missing values,

  for the \code{is.na} method, a \code{"timeSeries"} object whose data
  part is a logical matrix of the same dimension as in \code{x} indicating 
  if the corresponding values are \code{NA} or not.
  
}

\examples{
## Dummy 'timeSeries' with NAs entries
\dontshow{set.seed(2023)}
   data <- matrix(sample(c(1:20, rep(NA,4))), ncol = 2)
   s <- timeSeries(data, timeCalendar(2023))
   is.na(s)  
## Find the longest consecutive non-missing values
   na.contiguous(s)
}
