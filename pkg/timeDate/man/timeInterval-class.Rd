\name{timeInterval-class}
\docType{class}
\alias{timeInterval-class}

\alias{initialize,timeInterval-method}
%\alias{timeInterval,timeInterval,missing-method}

\title{Class 'timeInterval'}
\description{

  An object from class \code{"timeInterval"} represents a time interval or
  an union of time intervals. Methods are defined for union,
  intersection, complement and other suitable operations.  
  
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("timeInterval", ...)}
or, preferably, \code{\link{timeInterval}}.

A \code{"timeInterval"} object represents the union of zero or more
intervals of the form \code{[left, right)}, i.e., closed on the left and
open on the right (but see below the note about \code{left = -Inf}). The
internal representation is always in a canonical disjoint form, such
that the intervals do not overlap and do not touch at their end points.

The start of an interval can be \code{-Inf} and the end can be
\code{Inf}. When the left side of an interval is \code{-Inf}, it is
currently unspecified if \code{-Inf} belongs to it. In other words, it
is not defined whether \code{timeInterval[-Inf, b]} represents
\eqn{(-\infty, b)} or \eqn{[-\infty, b)}. Feedback on this will be
appreciated. The current code treats it as \eqn{[-\infty, b)}.  


}
\section{Slots}{
  \describe{
    \item{\code{left}:}{Object of class \code{"timeDate"} ~~ }
    \item{\code{right}:}{Object of class \code{"timeDate"} ~~ }
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "timeInterval")}: ... }
    \item{timeInterval}{\code{signature(left = "timeInterval", right = "missing")}: ... }
	 }
}

\author{
  Georgi N. Boshnakov
}
%
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{timeInterval}} for creation of 'timeInterval' objects and
  further examples
}

\examples{
showClass("timeInterval")
}

\keyword{classes}
\keyword{chron}
