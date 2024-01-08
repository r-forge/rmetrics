\name{format-methods}
\alias{format}

\alias{format.timeDate}


\title{Format methods}

\description{
    Formats \code{"timeDate"} objects as ISO conform character strings.
}

\usage{
\method{format}{timeDate}(x, format = "", tz = "", usetz = FALSE, \dots)
}

\arguments{
  \item{format}{
    a character string describing the format.
  }
  \item{tz}{
    a timezone specification to be used for the conversion. 
  }    
  \item{usetz}{
    a logical.
  }   
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{\dots}{
    arguments passed to other methods.
  }
}

\value{
  an ISO conforming formatted character string
}

\seealso{
  \code{as.character}
}

\examples{
## timeCalendar
# Time Calendar 16:00
tC = timeCalendar() + 16*3600
tC
   
## format as ISO character string 
format(tC)
}

\keyword{chron}
