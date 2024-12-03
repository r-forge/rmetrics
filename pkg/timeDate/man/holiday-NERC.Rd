\name{holidayNERC}
\alias{holidayNERC}


\title{NERC holiday calendar}

\description{
    
  Returns a holiday calendar for NERC, the North American Reliability
  Council.
                
}

\usage{
holidayNERC(year = getRmetricsOptions("currentYear"), FinCenter = "Eastern")
}

\arguments{
  \item{year}{
    an integer value or vector of years, formatted as \code{YYYY}.
  }
  \item{FinCenter}{
    a character value, the name of the financial center to use.
  }
}

\value{
  an object of class \code{"timeDate"}
}

\author{
    Joe W. Byers
}

\references{
    http://www.nerc.com/~oc/offpeaks.html
}

\seealso{
  \code{\link{holidayLONDON}},
  \code{\link{holidayNYSE}},
  \code{\link{holidayTSX}},
  \code{\link{holidayZURICH}} for holidays at other major financial centers.

  \code{\link{BoxingDay}}, etc., for descriptions of the individual
  holiday functions,

  \code{\link{listHolidays}} for a list (character vector) of all
  holidays,

  \code{\link{holiday}} alternative to calling directly individual
  holiday functions (takes one or more holiday functions as argument),
  
  \code{\link{Easter}}
}

\examples{
holidayNERC()
holidayNERC(2008:2010)
}

\keyword{chron}
