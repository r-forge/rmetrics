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


\value{
  an object of class \code{"timeDate"}
}


\examples{
## holidayZURICH -
   holidayZURICH()
   holidayZURICH(2008:2010)
}

\keyword{chron}
