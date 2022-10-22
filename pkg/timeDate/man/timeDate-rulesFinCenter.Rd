\name{rulesFinCenter}
\alias{rulesFinCenter}


\title{Financial centers DST rules}

\description{

  Returns DST rules for a financial center.

}

\usage{
rulesFinCenter(FinCenter = "")
}

\arguments{

  \item{FinCenter}{

    a character string with the location of the financial center named
    as "continent/city".

  }
}


\value{
  a list of time zones and DST rules available in the database
}


\details{

  The function \code{rulesFinCenter} lists the daylight saving rules for
  a selected financial center.
    
  There is no dependency on the POSIX implementation of your operating
  system because all time zone and day light saving time information is
  stored locally in ASCII files.
    
}

\seealso{
  \code{\link{listFinCenter}} for a list of the available financial
  centers
}

\examples{
## rulesFinCenter - 
   rulesFinCenter("Zurich")
}

\keyword{chron}
