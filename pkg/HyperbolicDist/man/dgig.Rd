\name{Generalized Inverse Gaussian}
\alias{dgig}
\alias{pgig}
\alias{qgig}
\alias{rgig}
\alias{rgig1}
\alias{ddgig}
\alias{gigBreaks}


\title{Generalized Inverse Gaussian Distribution}

\description{
  Density function, cumulative distribution function, quantile function
  and random number generation for the generalized inverse Gaussian
  distribution with parameter vector \code{Theta}. Utility routines are
  included for the derivative of the density function and to find
  suitable break points for use in determining the distribution function.
}

\usage{
dgig(x, Theta, KOmega = NULL)
pgig(q, Theta, small = 10^(-6), tiny = 10^(-10), deriv = 0.3,
     subdivisions = 100, accuracy = FALSE, ...)
qgig(p, Theta, small = 10^(-6), tiny = 10^(-10), deriv = 0.3,
     nInterpol = 100, subdivisions = 100, ...)
rgig(n, Theta)
rgig1(n, Theta)
ddgig(x, Theta, KOmega = NULL, ...)
gigBreaks (Theta, small = 10^(-6), tiny = 10^(-10), deriv = 0.3, ...)
}

\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations to be generated.}
  \item{Theta}{Parameter vector taking the form \code{c(lambda,chi,psi)}
    for \code{rgig}, or \code{c(chi,psi)} for \code{rgig1}.}
  \item{KOmega}{Sets the value of the Bessel function in the density or
    derivative of the density. See \bold{Details}.}
  \item{small}{Size of a small difference between the distribution
    function and zero or one. See \bold{Details}.}
  \item{tiny}{Size of a tiny difference between the distribution
    function and zero or one. See \bold{Details}.}
  \item{deriv}{Value between 0 and 1. Determines the point where the
    derivative becomes substantial, compared to its maximum value. See
    \bold{Details}.}
  \item{accuracy}{Uses accuracy calculated by \code{\link{integrate}}
    to try and determine the accuracy of the distribution function
    calculation.}
  \item{subdivisions}{The maximum number of subdivisions used to integrate
    the density returning the distribution function.}
  \item{nInterpol}{The number of points used in qhyperb for cubic spline
    interpolation (see \code{splinefun}) of the distribution function.}
  \item{\dots}{Passes arguments to \code{uniroot}.  See \bold{Details}.}
}

\details{
  The generalized inverse Gaussian distribution has density

  \deqn{f(x)=\frac{(\psi/\chi)^{\frac{\lambda}{2}}}%
    {2K_\lambda(\sqrt{\psi\chi})}x^{\lambda-1}%
    e^{-\frac{1}{2}\left(\chi x^{-1}+\psi x\right)}}{%
    f(x)=(psi/chi)^{lambda/2}/%
    (2K_lambda(sqrt(psi chi)))x^(lambda-1)%
    exp(-(1/2)(chi x^(-1)+psi x))}

   for \eqn{x>0}{x>0}, where \eqn{K_\lambda()}{K_lambda()} is the
   modified Bessel function of the third kind with order
   \eqn{\lambda}{lambda}.

  The generalized inverse Gaussian distribution is investigated in
  detail in \enc{Jörgensen}{Jorgensen} (1982).

  Use \code{gigChangePars} to convert from the
  \eqn{(\delta,\gamma)}{(delta,gamma)},
  \eqn{(\alpha,\beta)}{(alpha,beta)}, or
  \eqn{(\omega,\eta)}{(omega,eta)} parameterisations to the
  \eqn{(\chi,\psi)}{(chi,psi)}, parameterisation used above.

  \code{pgig} breaks the real line into eight regions in order to
  determine the integral of \code{dgig}. The break points determining
  the regions are found by \code{gigBreaks}, based on the values of
  \code{small}, \code{tiny}, and \code{deriv}. In the extreme tails of
  the distribution where the probability is \code{tiny} according to
  \code{gigCalcRange}, the probability is taken to be zero.  For the
  generalized inverse Gaussian distribution the leftmost breakpoint is
  not affected by the value of \code{tiny} but is always taken as 0. In
  the inner part of the distribution, the range is divided in 6 regions,
  3 above the mode, and 3 below. On each side of the mode, there are two
  break points giving the required three regions. The outer break point
  is where the probability in the tail has the value given by the
  variable \code{small}. The inner break point is where the derivative
  of the density function is \code{deriv} times the maximum value of the
  derivative on that side of the mode. In each of the 6 inner regions
  the numerical integration routine \code{\link{safeIntegrate}} (which
  is a wrapper for \code{\link{integrate}}) is used to integrate the
  density \code{dgig}.

  \code{qgig} use the breakup of the real line into the same 8
  regions as \code{pgig}. For quantiles which fall in the 2 extreme
  regions, the quantile is returned as \code{-Inf} or \code{Inf} as
  appropriate. In the 6 inner regions \code{splinefun} is used to fit
  values of the distribution function generated by \code{pgig}. The
  quantiles are then found using the \code{uniroot} function.

  \code{pgig} and \code{qgig} may generally be expected to be accurate
  to 5 decimal places. Unfortunately, when \code{lambda} is less than
  -0.5, the accuracy may be as little as 3 decimal places.

  Generalized inverse Gaussian observations are obtained via the
  algorithm of Dagpunar (1989).
}

\value{
  \code{dgig} gives the density, \code{pgig} gives the distribution
  function, \code{qgig} gives the quantile function, and \code{rgig}
  generates random variates. \code{rgig1} generates random variates in
  the special case where \eqn{\lambda=1}{lambda=1}. An estimate of the
  accuracy of the approximation to the distribution function may be
  found by setting \code{accuracy = TRUE} in the call to \code{phyperb}
  which then returns a list with components \code{value} and
  \code{error}.

  \code{ddgig} gives the derivative of \code{dgig}.

  \code{gigBreaks} returns a list with components:
  \item{xTiny}{Takes value 0 always.}
  \item{xSmall}{Value such that probability to the left is less than
    \code{small}.}
  \item{lowBreak}{Point to the left of the mode such that the
    derivative of the density is \code{deriv} times its maximum value
    on that side of the mode}
  \item{highBreak}{Point to the right of the mode such that the
    derivative of the density is \code{deriv} times its maximum value
    on that side of the mode}
  \item{xLarge}{Value such that probability to the right is less than
    \code{small}.}
  \item{xHuge}{Value such that probability to the right is less than
    \code{tiny}.}
  \item{modeDist}{The mode of the given generalized inverse Gaussian
    distribution.}
}

\references{
  Dagpunar, J.S. (1989). An easily implemented generalised inverse
  Gaussian generator. \emph{Commun. Statist. -Simula.}, \bold{18},
  703--710.
  
  \enc{Jörgensen}{Jorgensen}, B. (1982). \emph{Statistical Properties of
  the Generalized Inverse Gaussian Distribution}. Lecture Notes in
  Statistics, Vol. 9, Springer-Verlag, New York.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Richard Trendall,
  and Melanie Luen.}

\seealso{
  \code{\link{safeIntegrate}}, \code{\link{integrate}} for its
  shortfalls, \code{\link{splinefun}}, \code{\link{uniroot}} and
  \code{\link{gigChangePars}} for changing parameters to the
  \eqn{(\chi,\psi)}{(chi,psi)} parameterisation, \code{\link{dghyp}} for
  the generalized hyperbolic distribution.
}

\examples{
Theta <- c(1,2,3)
gigRange <- gigCalcRange(Theta, tol = 10^(-3))
par(mfrow = c(1,2))
curve(dgig(x, Theta), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Density of the\n Generalized Inverse Gaussian")
curve(pgig(x, Theta), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Distribution Function of the\n Generalized Inverse Gaussian")
dataVector <- rgig(500, Theta)
curve(dgig(x, Theta), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add =TRUE)
title("Density and Histogram\n of the Generalized Inverse Gaussian")
logHist(dataVector, main =
   "Log-Density and Log-Histogram\n of the Generalized Inverse Gaussian")
curve(log(dgig(x, Theta)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)
par(mfrow = c(2,1))
curve(dgig(x, Theta), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Density of the\n Generalized Inverse Gaussian")
curve(ddgig(x, Theta), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Derivative of the Density\n of the Generalized Inverse Gaussian")
par(mfrow = c(1,1))
gigRange <- gigCalcRange(Theta, tol = 10^(-6))
curve(dgig(x, Theta), from = gigRange[1], to = gigRange[2],
      n = 1000)
bks <- gigBreaks(Theta)
abline(v = bks)
}

\keyword{distribution}
