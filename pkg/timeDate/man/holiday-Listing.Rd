\name{listHolidays}
\alias{listHolidays}


\title{List of holidays}

\description{
  Returns a list of holidays supported by package \code{"timeDate"}.
}

\usage{
listHolidays(pattern = ".*") 
}

\arguments{
  \item{pattern}{
    a character string containing a regular expression.
  }
}

\details{

  Gives a character vector containing the names of supported holidays
  matching \code{pattern}. The default is to return all holidays.

  The list is sorted alphabetically. It is changed from time to
  time. So, the use of character indexing (possibly representing
  patterns) on the returned list is strongly recommended.

}

\value{
  a character vector, sorted in alphabetical order
}

\seealso{
  \code{\link{BoxingDay}}, etc., for descriptions of the individual
  holiday functions,

  \code{\link{holiday}} alternative to calling directly individual
  holiday functions (takes one or more holiday functions as argument),
  
  \code{\link{Easter}},

  \code{\link{holidayLONDON}},
  \code{\link{holidayNERC}},
  \code{\link{holidayNYSE}},
  \code{\link{holidayTSX}},
  \code{\link{holidayZURICH}} for holidays at major financial centers.
}
\examples{
## Local Swiss Holidays:
listHolidays("CH")

listHolidays("Easter")
listHolidays("NewYear")

## All Holidays
listHolidays()
}

\keyword{chron}
