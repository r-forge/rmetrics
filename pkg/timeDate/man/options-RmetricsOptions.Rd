\name{RmetricsOptions}

\alias{setRmetricsOptions}
\alias{getRmetricsOptions}

\alias{getRmetricsOption}


\title{Rmetrics option settings}

\description{

  Allow the user to set and examine a variety of global options which
  affect the way in which Rmetrics functions compute and display their
  results.

}

\usage{
setRmetricsOptions(\dots)
getRmetricsOptions(x, unset = "")
}

\arguments{
  \item{unset}{
    a character string holding the return value is x is not set.
  }
  \item{x}{
    a character string holding an option name.
  }
  \item{\dots}{
    any options can be defined, using \code{name = value} or by
    passing a list of such tagged values.
  }
}
