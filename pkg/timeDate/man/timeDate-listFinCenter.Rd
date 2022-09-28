\name{listFinCenter}


\title{List of Financial Centers}


\alias{listFinCenter}


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


\value{

  a character vector listing the financial centers whose names match
  \code{pattern}.

}


\seealso{
  \code{\link{rulesFinCenter}} for the daylight saving rules
}

\examples{
## myFinCenter - the global setting currently used:
   getRmetricsOptions("myFinCenter")

## Other Financial Centers:
   listFinCenter("Asia/")
   listFinCenter("^A")    # all beginning with "A"
   listFinCenter("^[^A]") # all *not* beginning with "A"
   listFinCenter(".*/L")  # cities with L*

   stopifnot(identical(sort(listFinCenter()), ## 'A' and 'not A' == everything:
        sort(union(listFinCenter("^A"),
            listFinCenter("^[^A]")))))
}

\keyword{chron}
