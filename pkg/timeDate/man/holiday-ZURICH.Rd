\name{holidayZURICH}
\alias{holidayZURICH}


\title{Zurich holiday calendar}

\description{
  Returns a holiday calendar for Zurich.     
}

\usage{
holidayZURICH(year = getRmetricsOptions("currentYear"))
}

\arguments{
  \item{year}{
    an integer value or vector of years, formatted as \code{YYYY}.
  }
}

\details{

  The Zurich holiday calendar includes the following holidays:
  NewYearsDay, GoodFriday, EasterMonday, 
  LaborDay, PentecostMonday, ChristmasDay, 
  BoxingDay, CHBerchtoldsDay, CHSechselaeuten, 
  CHAscension, CHConfederationDay, CHKnabenschiessen.

}

\seealso{
  \code{\link{holidayLONDON}},
  \code{\link{holidayNERC}},
  \code{\link{holidayNYSE}},
  \code{\link{holidayTSX}},
  for holidays at other major financial centers,

  \code{\link{BoxingDay}}, etc., for descriptions of the individual
  holiday functions,

  \code{\link{listHolidays}} for a list (character vector) of all
  holidays,

  \code{\link{holiday}} alternative to calling directly individual
  holiday functions (takes one or more holiday functions as argument),
  
  \code{\link{Easter}}
}

\value{
  an object of class \code{"timeDate"}
}

\examples{
holidayZURICH()
holidayZURICH(2008:2010)
}

\keyword{chron}
