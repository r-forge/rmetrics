\name{in_int}
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
  \code{"timeInterval"}.

}

\usage{
\method{!}{timeInterval}(x)
\S4method{&}{timeInterval,timeInterval}(e1,e2)
\S4method{|}{timeInterval,timeInterval}(e1,e2)
\S4method{^}{timeInterval,timeInterval}(e1,e2)

x \%in_int\% ti
\S4method{\%in_int\%}{timeInterval,timeInterval}(x,ti)
\S4method{\%in_int\%}{timeDate,timeInterval}(x,ti)
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

  \code{x \%in_int\% ti} checks if the \code{x} is in the
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

  \code{timeInterval} for creating \code{"timeInterval"} objects.
}
\examples{

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
