\name{listFinCenter}
\alias{listFinCenter}


\title{List of financial centers}

\description{
  Lists supported financial centers. 
}

\usage{
listFinCenter(pattern = ".*")
}

\arguments{
  \item{pattern}{
    
    a pattern character string as required by the \code{\link{grep}}
    function. The default, \code{".*"}, gives all supported financial
    centers

  }
}

\details{

  The list returned by \code{listFinCenter} doesn't contain all
  financial centers supported by \pkg{timeDate}. Rather it contains
  currently supported \sQuote{standard names} of time zones defined in
  the tz (a.k.a. Zoneinfo) database.  Names supported by previous
  versions of by \pkg{timeDate} are recognised, even though they are not
  in the list.
  
}

\value{

  a character vector listing the financial centers whose names match
  \code{pattern}.

}

\seealso{
  \code{\link{rulesFinCenter}} for the daylight saving rules
}

\examples{
## myFinCenter - the global setting currently used
getRmetricsOptions("myFinCenter")

## Other Financial Centers
listFinCenter("Asia/")
listFinCenter("^A")    # all beginning with "A"
listFinCenter("^[^A]") # all *not* beginning with "A"
listFinCenter(".*/L")  # cities with L*

stopifnot(identical(sort(listFinCenter()), ## 'A' and 'not A' == everything:
     sort(union(listFinCenter("^A"),
         listFinCenter("^[^A]")))))
}

\keyword{chron}
