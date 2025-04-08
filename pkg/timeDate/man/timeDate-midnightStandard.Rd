\name{midnightStandard}
\alias{midnightStandard}

\alias{midnightStandard2}


\title{Midnight standard}

\description{

  Corrects \code{"timeDate"} objects if they do not fulfill the ISO8601
  midnight standard.

}

\usage{
midnightStandard (charvec, format)
midnightStandard2(charvec, format)
}

\arguments{
  \item{charvec}{
    a character string or vector of dates and times.
  }
  \item{format}{
    a string, the format specification of the input character vector.
  }
}

\details{

  \code{midnightStandard2()} calls \code{\link{strptime}}. Since the
  latter returns \code{NA}s for elements that don't conform to the
  midnight standard, the inputs corresponding to \code{NA}s are further
  processed to fix this.

  \code{midnightStandard()} converts to character vector the result
  obtained from \code{midnightStandard2()}.

}

\value{
  for \code{midnightStandard}, a \code{character} vector,
  
  for \code{midnightStandard2}, a \code{\link{POSIXct}} object with time
  zone \code{"GMT"}.

}

\seealso{
  \code{\link{whichFormat}}
}
\examples{
ch <- "2007-12-31 24:00"
midnightStandard(ch)
(ms2 <- midnightStandard2(ch))
class(ms2)
}

\keyword{chron}
