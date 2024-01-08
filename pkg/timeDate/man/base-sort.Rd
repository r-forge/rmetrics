\name{sort}
\alias{sort}

\alias{sort.timeDate}


\title{Sorting 'timeDate' objects}

\description{
  Sorts a \code{"timeDate"} object.
}

\usage{
\method{sort}{timeDate}(x, \dots)
}

\arguments{
  \item{x}{
    an object of class \code{"timeDate"}.
  }  
  \item{\dots}{
    arguments passed to other methods.
  }
}

\value{
  an object of class \code{"timeDate"}
}

\examples{
## c
# create character vectors
dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
dts
tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
tms
   
## "+/-"
# add one day to a given timeDate object:
GMT = timeDate(dts, zone = "GMT", FinCenter = "GMT")
GMT
ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")
ZUR
   
## c
# concatenate and replicate timeDate objects
c(GMT[1:2], ZUR[1:2])
c(ZUR[1:2], GMT[1:2])
   
## rep
rep(ZUR[2], times = 3)
rep(ZUR[2:3], times = 2)  
}

\keyword{chron}
