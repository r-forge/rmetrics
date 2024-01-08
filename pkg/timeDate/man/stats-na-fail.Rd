\name{is.na-methods}
\alias{is.na-methods}

\docType{methods}

\alias{is.na,timeDate-method}


\title{Methods for 'is.na'}

\description{

  \code{is.na} methods for \code{"timeDate"} objects.

}


\examples{
# create a timeCalendar sequence
(td <- timeCalendar())
is.na(td)

# insert NA's
is.na(td) <- 2:3
td

# test of NA's
is.na(td)
}

\keyword{chron}
