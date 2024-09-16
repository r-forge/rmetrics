\name{summary-methods}

\alias{summary.timeDate}
\alias{timeDate_summary}
\alias{print.timeDate_summary}

\alias{timeDate_summary}
\alias{print.timeDate_summary}

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
  an object from S3 class \code{"timeDate_summary"}, which has a print
  method
}

\examples{
tC <- timeCalendar()
summary(tC)
}

\keyword{chron}
