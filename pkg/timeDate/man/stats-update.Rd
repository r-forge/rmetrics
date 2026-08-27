\name{update.timeDate}
\alias{update.timeDate}

\alias{update}


\title{Update a timeDate object}

\description{Update a \sQuote{timeDate} object.}

\usage{
\method{update}{timeDate}(object, zone = NULL, FinCenter = zone, dst_gap = "+", ...)
}

\arguments{
  \item{object}{
    a \code{"timeDate"} object.
  }
  \item{zone}{
    a character string,
    the time zone or financial center where the data were recorded,
    see section \sQuote{Details}.
  }
  \item{FinCenter}{
    a character string,
    a time zone or financial center.
  }
  \item{dst_gap}{
    a character string specifying what to do with non-existent times
    falling in a DST gap, see \code{\link{timeDate}}.
  }
  \item{\dots}{Ignored.}
}

\details{

  Argument \code{zone} requests to change the datetimes, so that if the
  slot \code{FinCenter} of the modified object is set to \code{zone}, the
  printed times will be (almost) the same as those of the input
  object. This is useful, for example, when we want to reset the time
  zone of timedates returned by other functions. For example, most
  functions in this package return holidays in GMT time zone but users
  may prefer them in their time zone.

  The qualifier \sQuote{almost} in the above paragraph is needed because
  some datetimes in \code{object} may not exist in the target zone if DST
  is in operation.

  If \code{zone} is not \code{NULL}, it must be a character string
  designating one of the supported time zones or financial centers.  It
  can also be the empty string, in which case the zone is set to the
  value returned by \code{getRmetricsOptions("myFinCenter")}.

  Argument \code{FinCenter} can be used to set the \code{FinCenter} slot
  of the returned object. The defult is to \code{zone} if that is not
  null \code{NULL}, otherwise the \code{FinCenter} slot of the result
  will be left as is.

  See also the \code{"character"} method of \code{\link{timeDate}}.

  You can reduce the need for updates of this kind by setting suitable
  Rmetrics options, see the examples.

}

\value{
  an object of class \code{"timeDate"}
}  

\author{Georgi N. Boshnakov}

\note{
  Currently this method is mainly useful to reset the \code{zone}.
  Suggestions for other features that can be usefully updated are
  welcome. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{timeDate}}
}

\examples{
## New Years Day
ea1 <- NewYearsDay(2026)
ea1

cat("What is the time in Jamaica when New Year arrives in ", finCenter(ea1), "?\n")
ea2 <- ea1
ea2@FinCenter <- "Jamaica"
ea2

## New Years Day in Jamaica
ea3 <- update(ea1, zone = "Jamaica")
ea3

## set the Rmetrics option "myFinCenter" to get most results directly in it.
savop <- getRmetricsOptions("myFinCenter")

setRmetricsOptions(myFinCenter = "Jamaica")
NewYearsDay(2026) # the result is directly for Jamaica

## restore the previous value of the option
setRmetricsOptions(myFinCenter = savop)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
