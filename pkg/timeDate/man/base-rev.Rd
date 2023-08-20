\name{rev}
\alias{rev}

\alias{rev.timeDate}


\title{Reverse 'timeDate' objects}

\description{
  Reverse a \code{"timeDate"} object.
}

\usage{
\method{rev}{timeDate}(x)
}

\arguments{
      
  \item{x}{
    an object of class \code{"timeDate"}.
  }  
}

\value{
  an object of class \code{"timeDate"}
}

\examples{
dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")
ZUR
rev(ZUR)
}

\keyword{chron}
