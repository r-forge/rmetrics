\name{utils-download}

\alias{composeURL}
\alias{indexGrep}

\title{Utilities for composing URL's}

\description{
    Two helpful utilities for assembling URL's.
}

\usage{
composeURL(\dots, prefix="http://")
indexGrep(pattern, x, \dots)
}

\arguments{

  \item{\dots}{
    for \code{composeURL}, character strings from which the URL will
    be composed; for \code{indexGrep}, optional arguments to be
    passed to the function \code{grep}.
  }

  \item{prefix}{
    a character string specifying the prefix of the URL.
  }

  \item{pattern}{
    a character string containing a regular expression to be matched in
    the given character vector.
  }

  \item{x}{
    a character vector where matches are sought.
  }
}


\references{

Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
    
}

\keyword{programming}
