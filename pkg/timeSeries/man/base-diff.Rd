\name{diff}                     
\alias{diff}

\alias{diff,timeSeries-method}
                                    
                                    
\title{diff}

\description{

    Difference a \code{"timeSeries"} object.
    
}


\usage{
diff(x, \dots)
}

% lag=1, diff=1, trim=FALSE, pad=NA,

\arguments{
  
  \item{x}{
    an object of class \code{"timeSeries"}.
  }
   
  \item{\dots}{
    further arguments to be passed, see Section \sQuote{Details}.
  }
      
}


\details{

    Arguments to be passed may include:\cr
    
    \code{lag} - an integer indicating which lag to use. 
        By default 1.\cr
    \code{diff} - an integer indicating the order of the difference. 
        By default 1.\cr
    \code{trim} - a logical flag. Should NAs at the beginning of the 
      series be removed? By default FALSE.\cr
    \code{pad} - a umeric value with which NAs should be replaced at 
      the beginning of the series. By default NA.

}

 
\value{

  the differenced \code{"timeSeries"} object

}


\examples{
## Load Microsoft Data Set -
   x <- MSFT[1:12, ]
   x

## Compute Differences -
   diff(x)   
   
## Trimmed Differences -
   diff(x, trim=TRUE) 

## Padded Differences -
   diff(x, trim=FALSE, pad=0) 
}


\keyword{chron}
\keyword{ts}
