\name{RmetricsOptions}

\alias{setRmetricsOptions}
\alias{getRmetricsOptions}


\title{Rmetrics option settings}

\description{

  Allow the user to set and examine a variety of global options which
  affect the way in which Rmetrics functions compute and display their
  results.

}

\usage{
getRmetricsOptions(x, unset = "")
setRmetricsOptions(\dots)
}

\arguments{
  \item{x}{
    a character vector of option names. If missing, give all Rmetrics
    options currently set.
  }
  \item{unset}{
    a character vector or a list specifying what values to return for
    options in \code{x} that are not set (see \code{\link{mget}}).
  }
  \item{\dots}{
    any options can be defined using \code{name = value} or by passing a
    list of such tagged values.
  }
}

\value{
  for \code{getRmetricsOptions}, the value(s) of the requested options,
  
  for \code{setRmetricsOptions}, the revious value(s) of the options set
  in the call, invisibly,
}
