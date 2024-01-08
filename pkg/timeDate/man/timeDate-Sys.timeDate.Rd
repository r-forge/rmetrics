\name{Sys.timeDate}
\alias{Sys.timeDate}


\title{System time as 'timeDate' object}

\description{
  
  Returns the system time as an object of class \code{"timeDate"}.

}

\usage{
Sys.timeDate(FinCenter = "")
}

\arguments{
  \item{FinCenter}{

    a character string with the location of the financial center named
    as "continent/city".

  }
}

\value{
  a \code{"timeDate"} object
}

\examples{
\dontrun{
## direct
Sys.timeDate()

## Local Time in Zurich
Sys.timeDate(FinCenter = "Zurich")  

## transformed from "POSIX(c)t" with timeDate()
timeDate(Sys.time()) 
 
## Local Time in Zurich
timeDate(Sys.time(), FinCenter = "Zurich")  
}   
}

\keyword{chron}
