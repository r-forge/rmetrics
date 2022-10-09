\name{finCenter}


\title{Get and set Financial center of a 'timeSeries'}


\alias{getFinCenter}  
\alias{setFinCenter<-}

\alias{finCenter,timeSeries-method}
\alias{finCenter<-,timeSeries-method}


\description{

    Get or assign a financial center to a 'timeSeries' object.
    
}


\usage{
\S4method{finCenter}{timeSeries}(x)
\S4method{finCenter}{timeSeries}(x) <- value

getFinCenter(x)         
setFinCenter(x) <- value
}


\arguments{
  
    \item{x}{
        a 'timeSeries' object.
        }
    \item{value}{
        a character with the the location of the financial center named 
        as \code{"continent/city"}.
        }
}


\seealso{
  \code{\link{listFinCenter}}
}


\examples{
## An artificial timeSeries Object - 
   tS <- dummySeries()
   tS

## Print Financial Center -
   finCenter(tS)
   getFinCenter(tS)

## Assign New Financial Center - 
   finCenter(tS) <- "Zurich"
   tS
   setFinCenter(tS) <- "New_York"
   tS
}


\keyword{programming}

