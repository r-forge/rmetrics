\name{myFinCenter}
\alias{myFinCenter}


\title{myFinCenter variable}

\description{

  A character string with the name of my financial center.
    
}

\note{

    Can be modified by the user to his/her own or any other financial
    center. The default is \code{"GMT"}.  To list all supported
    financial centers use the function \code{listFinCenter}.
   
}


\seealso{
    \code{\link{listFinCenter}}
}

\examples{
## myFinCenter - the global setting currently used:
   getRmetricsOptions("myFinCenter")
   
## Change to another Financial center:
   # setRmetricsOptions(myFinCenter = "Zurich")
   
## Do not care about DST ...
   # setRmetricsOptions(myFinCenter = "GMT") 
}

\keyword{chron}
