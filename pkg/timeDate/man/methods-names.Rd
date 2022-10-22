\name{names-methods}
\alias{names-methods}

\docType{methods}

\alias{names,timeDate-method}
\alias{names<-,timeDate-method}


\title{
  The names of a 'timeDate' object
}

\description{
  Functions to get or set the names of a \code{"timeDate"} object.
}

\usage{
\S4method{names}{timeDate}(x)
\S4method{names}{timeDate}(x) <- value
}

\arguments{
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{value}{
    a character vector of up to the same length as 'x', or 'NULL'.
  }
}

\examples{
td <- timeCalendar()
td
names(td) <- LETTERS[seq_along(td)]
td
}
