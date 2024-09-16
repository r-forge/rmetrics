\name{listHolidays}
\alias{listHolidays}


\title{List of holidays}

\description{
  Returns a list of holidays.     
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

  Returns a character vector containing the names of supported holidays
  matching \code{pattern}. The default is to return all holidays.

  The list is sorted alphabetically. It is changed from time to
  time. So, the use of character indexing (possibly representing
  patterns) on the returned list is strongly recommended.

}

\value{
  a character vector
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
