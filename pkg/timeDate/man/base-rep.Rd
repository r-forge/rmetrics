\name{rep}
\alias{rep}
\alias{rep.timeDate}

\title{Replicating 'timeDate' Objects}

\description{
    
  Replicate 'timeDate' objects.
       
}

\usage{
\method{rep}{timeDate}(x, \dots)
}

\arguments{
      
  \item{x}{
    an object of class \code{timeDate}.
  }  
  \item{\dots}{
    arguments passed to the method for \code{'POSIXct'},
    \code{\link[base]{rep}}.
  }
       
}

\value{
    
    an object of class \code{"timeDate"}.

}

\examples{
## rep - 
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")

   rep(ZUR[2], times = 3)
   rep(ZUR[2:3], times = 2)  
}

\keyword{chron}

