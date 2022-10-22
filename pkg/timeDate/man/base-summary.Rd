\name{summary-methods}
\alias{summary.timeDate}


\title{Summary method}

\description{

  Summarizes details of a \code{"timeDate"} object.

}


\usage{
\method{summary}{timeDate}(object, \dots)
}

\arguments{

    \item{object}{
        an object of class \code{"timeDate"}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\details{

    Creates a summary report of the details of a \code{"timeDate"}
    object. This includes the starting and end date, the number of dates
    the format and the financial center in use.
   
}

\value{

  Currently the function just prints the report and returns invisibly
  the object.
  \cr
  
  \bold{TODO:} The return value return value should not be relied upon
  since it is inconsistent with the way other methods for \code{summary}
  work. They create an object with a print method and returned the
  object, which is typically printed but can also be assigned.

}

\examples{
## summary -
   tC = timeCalendar()
   summary(tC)
}

\keyword{chron}
