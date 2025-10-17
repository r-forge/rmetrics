\name{in_int}
\alias{in_int}
\alias{\%in_int\%}

\alias{!}
\alias{!.timeInterval}

\alias{&-methods}
\alias{&,timeInterval,timeInterval-method}

\alias{|-methods}
\alias{|,timeInterval,timeInterval-method}

\alias{^-methods}
\alias{^,timeInterval,timeInterval-method}


\alias{\%in_int\%-methods}
\alias{\%in_int\%,ANY,ANY-method}
\alias{\%in_int\%,timeDate,timeInterval-method}
\alias{\%in_int\%,timeInterval,timeInterval-method}

\alias{tiInf}

\concept{union}
\concept{intersection}
\concept{complement}
\concept{set difference}
\concept{element}

\title{
  Set operations on timeInterval objects
}

\description{

  Union, intersection, complement and set difference of
  \code{"timeInterval"} objects. Also testing if an object is in a
  \code{"timeInterval"} and \code{tiInf} representing the infinite time
  interval.

}

\usage{
\method{!}{timeInterval}(x)
\S4method{&}{timeInterval,timeInterval}(e1,e2)
\S4method{|}{timeInterval,timeInterval}(e1,e2)
\S4method{^}{timeInterval,timeInterval}(e1,e2)

x \%in_int\% ti
\S4method{\%in_int\%}{timeInterval,timeInterval}(x,ti)
\S4method{\%in_int\%}{timeDate,timeInterval}(x,ti)

tiInf
}

\arguments{
  \item{x}{
    a \code{"timeInterval"} or \code{"timeDate"} object
  }
  \item{e1,e2,ti}{
    \code{"timeInterval"} objects
  }
}

\details{

  Let \code{ti1} and \code{ti2} be objects from class
  \code{"timeInterval"}.

  \code{ti1 & ti2} is the intersection of \code{ti1} and \code{ti2},
  \code{ti1 | ti2} their union,
  \code{ti1} the complement of \code{ti1} (w.r.t. \code{(-Inf, Inf)}).

  \code{ti1 ^ ti2} is the complement of \code{ti1} w.r.t. \code{ti2},
  i.e., the set difference \code{ti2}/\code{ti1} (all points in
  \code{ti2} that are not in \code{ti1}). \code{!ti1} is the same as
  \code{ti1 ^ timeInterval(left = -Inf, right = Inf)}.

  \code{tiInf} represents the time interval from \code{-Inf} to
  \code{Inf}. 

  A motivation for using the logical operators for set operations is
  that in mathematics the notation for them is similar and the
  properties of logical \sQuote{and}, \sQuote{or} and \sQuote{negation}
  are analogous to those of intersection, union and complement. Also
  \eqn{A^c} is one of the notations for complement, where \sQuote{c}
  stands for the universe (the whole set w.r.t. which the complement is
  taken). Here, it corresponds to
  \code{timeInterval(left = -Inf, right = Inf)}. We extend this notation
  to allow taking a complement w.r.t. any \code{"timeInterval"} (i.e., a
  set difference).

  \code{x \%in_int\% ti} checks if \code{x} is in the
  \code{"timeInterval"} \code{ti}.

  If \code{x} is a \code{"timeInterval"} object, the result of
  \code{x \%in_int\% ti} is a single \code{TRUE} value if \code{x} is a
  sub-interval of \code{ti}; \code{FALSE} otherwise.

  If \code{x} is a \code{"timeDate"} object, the result of
  \code{x \%in_int\% ti} is a logical vector indicating which times are
  in \code{ti}.

}

\value{
  for \code{"&"}, \code{"|"}, \code{"!"} and \code{"^"} methods - a
  \code{"timeInterval"} object,
}

\author{Georgi N. Boshnakov}

%\note{
%%%  ~~further notes~~
%}
%
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  class \code{"\linkS4class{timeInterval}"},

  \code{\link{timeInterval}} for creation of \code{"timeInterval"} objects and
  further examples,

  \code{\link{in_int}} for set operations on \code{"timeInterval"} objects
}
\examples{
## create a time interval by rounding to the enclosing hour
ti <- timeInterval(timeDate("2024-12-20 10:20:30"), unit = "hours")
ti

## a similar interval on the following day
tib <- timeInterval(timeDate("2024-12-21 10:20:30"), unit = "hours")
tib

ti2 <- ti | tib
ti2

ti & ti2
ti | ti2

!ti
!ti2

identical(!ti, ti ^ timeInterval(left = -Inf, right = Inf))
identical(!ti2, ti2 ^ timeInterval(left = -Inf, right = Inf))

## tiInf represents the time interval (-Inf, Inf)
identical(tiInf, timeInterval(left = -Inf, right = Inf))

ti ^ ti2 # ti2 \ ti
ti2 ^ ti # ti  \ ti2

timeDate("2024-12-20 10:20:30") \%in_int\% ti2  # TRUE
timeDate("2024-12-20 11:20:30") \%in_int\% ti2  # FALSE

timeDate(c("2024-12-20 10:20:30", "2024-12-20 11:20:30")) \%in_int\% ti2  

## ti's are closed on the left and open on the right, hence:
ti2@left \%in_int\% ti2    # [1] TRUE TRUE
ti2@right \%in_int\% ti2   # [1] FALSE FALSE

## a timeInterval is a scalar, so the following give a single TRUE/FALSE
## indicating whether or not the first interval is contained in the second
ti \%in_int\% ti2
ti2 \%in_int\% ti
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{chron}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
