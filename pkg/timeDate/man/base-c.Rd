\name{c}
\alias{c}

\alias{c.timeDate}


\title{Concatenating 'timeDate' objects}

\description{
    Concatenates \code{"timeDate"} objects.
}

\usage{
\method{c}{timeDate}(\dots, recursive = FALSE)
}

\arguments{
  \item{recursive}{
    a logical. If recursive  is set to \code{TRUE}, the function 
    recursively descends through lists combining all their elements 
    into a vector. 
  }  
  \item{\dots}{
    arguments passed to other methods.
  }
}

\value{
  an object of class \code{"timeDate"}
}

\examples{
## timeCalendar
# Create Character Vectors:
GMT = timeCalendar(zone = "GMT", FinCenter = "GMT") + 16*3600
ZUR = timeCalendar(zone = "GMT", FinCenter = "Zurich") + 16*3600
   
## c
# concatenate and replicate timeDate objects
sort(c(GMT, ZUR))
sort(c(ZUR, GMT))
}

\keyword{chron}
