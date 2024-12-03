\name{holidayTSX}
\alias{holidayTSX}


\title{TSX holiday calendar}

\description{

  Returns a holiday calendar for the Toronto Stock Exchange.     

}

\usage{
holidayTSX(year = getRmetricsOptions("currentYear"))
}

\arguments{
  \item{year}{
    an integer value or vector of years, formatted as \code{YYYY}.
  }
}

\value{
  an object of class \code{"timeDate"}
}

\seealso{
  \code{\link{holidayLONDON}},
  \code{\link{holidayNERC}},
  \code{\link{holidayNYSE}},
  \code{\link{holidayZURICH}} for holidays at other major financial centers,

  \code{\link{BoxingDay}}, etc., for descriptions of the individual
  holiday functions,

  \code{\link{listHolidays}} for a list (character vector) of all
  holidays,

  \code{\link{holiday}} alternative to calling directly individual
  holiday functions (takes one or more holiday functions as argument),
  
  \code{\link{Easter}}
}

\examples{
holidayTSX()
holidayTSX(2008:2010)
}

\keyword{chron}
