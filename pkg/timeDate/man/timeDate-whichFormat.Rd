\name{whichFormat}
\alias{whichFormat}


\title{Format recognition}

\description{
  Tries to recognize the date/time format.
}

\usage{
whichFormat(charvec, silent = FALSE)  
}

\arguments{
  \item{charvec}{
    a character string or vector of dates and times.
  }
  \item{silent}{
    a logical flag. Should a warning be printed if the format cannot be
    recognized?
  }
}

\value{
  a format string
}

\seealso{
  \code{\link{midnightStandard}}
}

\examples{
whichFormat("2007-12-31 00:00:00")
whichFormat("2007-12-31 24:00")
}

\keyword{chron}
