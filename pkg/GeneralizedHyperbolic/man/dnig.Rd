\name{NIG}
\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}
\alias{ddnig}

\title{Normal Inverse Gaussian Distribution}

\description{
  Density function, distribution function, quantiles and
  random number generation for the normal inverse Gaussian distribution
  with parameter vector \code{param}. Utility routines are included for
  the derivative of the density function and to find suitable break
  points for use in determining the distribution function.
}

\usage{
dnig(x, mu = 0, delta = 1, alpha = 1, beta = 0,
     param = c(mu, delta, alpha, beta))
pnig(q, mu = 0, delta = 1, alpha = 1, beta = 0,
     param = c(mu, delta, alpha, beta),
     lower.tail = TRUE, subdivisions = 100,
     intTol = .Machine$double.eps^0.25, valueOnly = TRUE, ...)
qnig(p, mu = 0, delta = 1, alpha = 1, beta = 0,
     param = c(mu, delta, alpha, beta),
     lower.tail = TRUE, method = c("spline","integrate"),
     nInterpol = 501, uniTol = .Machine$double.eps^0.25,
     subdivisions = 100, intTol = uniTol, ...)
rnig(n, mu = 0, delta = 1, alpha = 1, beta = 0,
     param = c(mu, delta, alpha, beta))
ddnig(x, mu = 0, delta = 1, alpha = 1, beta = 0,
     param = c(mu, delta, alpha, beta))
}

\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations to be generated.}
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the
    distribution. A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default
    value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default
    this is 0.}
  \item{param}{Parameter vector taking the form
    \code{c(mu, delta, alpha, beta)}.}
  \item{method}{Character. If \code{"spline"} quantiles are found from a
    spline approximation to the distribution function. If
    \code{"integrate"}, the distribution function used is always obtained
    by integration.}
  \item{lower.tail}{Logical. If \code{lower.tail = TRUE}, the cumulative
    density is taken from the lower tail.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density and determine the accuracy of the distribution
    function calculation.}
  \item{intTol}{Value of \code{rel.tol} and hence \code{abs.tol} in
    calls to \code{integrate}. See \code{\link{integrate}}.}
  \item{valueOnly}{Logical. If \code{valueOnly = TRUE} calls to
    \code{pghyp} only return the value obtained for the integral.
    If \code{valueOnly = FALSE} an estimate of the
    accuracy of the numerical integration is also returned.}
  \item{nInterpol}{Number of points used in \code{qghyp} for cubic
    spline interpolation of the distribution function.}
  \item{uniTol}{Value of \code{tol} in
    calls to \code{uniroot}. See \code{\link{uniroot}}.}
  \item{\dots}{Passes arguments to \code{uniroot}. See \bold{Details}.}
}

\details{
  The normal inverse Gaussian distribution has density
  \deqn{e^{\delta \sqrt{\alpha^2 - \beta^2}}%
        \frac{\alpha \delta}{\pi \sqrt{\delta^2 + (x - \mu)^2}}%
        K_1(\alpha \sqrt{\delta^2 + (x - \mu)^2})%
        e^{\beta (x - \mu)}}{ %
        f(x) = exp(delta sqrt(alpha^2 - beta^2)%
        alpha delta/pi sqrt(delta^2 + (x - mu)^2)%
        K_1(alpha sqrt(delta^2 + (x - mu)^2))%
        exp(beta (x - mu))}


  where \eqn{K_1()}{K_1()} is the modified Bessel function of the
  third kind with order 1.

  A succinct description of the normal inverse Gaussian distribution is
  given in Paolella(2007). Because both of the normal inverse Gaussian
  distribution and the hyperbolic distribution are special cases of the
  generalized hyperbolic distribution (with different values of
  \eqn{\lambda}{lambda}), the normal inverse Gaussian distribution has
  the same sets of parameterizations as the hyperbolic distribution.
  And therefore one can use \code{hyperbChangePars} to interchange between
  different parameterizations for the normal inverse Gaussian distribution as
  well (see \code{hyperbChangePars} for details).

  Each of the functions are wrapper functions for their equivalent
  generalized hyperbolic distribution. For example, \code{dnig} calls
  \code{dghyp}.

  \code{pnig} breaks the real line into eight regions in order to
  determine the integral of \code{dnig}. The break points determining
  the regions are found by \code{nigBreaks}, based on the values of
  \code{small}, \code{tiny}, and \code{deriv}. In the extreme tails of
  the distribution where the probability is \code{tiny} according to
  \code{nigCalcRange}, the probability is taken to be zero. In the range
  between where the probability is \code{tiny} and \code{small}
  according to \code{nigCalcRange}, an exponential approximation to the
  hyperbolic distribution is used. In the inner part of the
  distribution, the range is divided in 4 regions, 2 above the mode, and
  2 below. On each side of the mode, the break point which forms the 2
  regions is where the derivative of the density function is
  \code{deriv} times the maximum value of the derivative on that side of
  the mode. In each of the 4 inner regions the numerical integration
  routine \code{\link[DistributionUtils]{safeIntegrate}} (which is a
  wrapper for \code{\link{integrate}}) is used to integrate the density
  \code{dnig}.

  \code{qnig} uses the breakup of the real line into the same 8
  regions as \code{pnig}. For quantiles which fall in the 2 extreme
  regions, the quantile is returned as \code{-Inf} or \code{Inf} as
  appropriate. In the range between where the probability is \code{tiny}
  and \code{small} according to \code{nigCalcRange}, an exponential
  approximation to the hyperbolic distribution is used from which the
  quantile may be found in closed form. In the 4 inner regions
  \code{splinefun} is used to fit values of the distribution function
  generated by \code{pnig}.  The quantiles are then found
  using the \code{uniroot} function.

  \code{pnig} and \code{qnig} may generally be expected to be
  accurate to 5 decimal places.

  Recall that the normal inverse Gaussian distribution is a special case
  of the generalized hyperbolic distribution and the generalized
  hyperbolic distribution can be represented as a particular mixture of
  the normal distribution where the mixing distribution is the
  generalized inverse Gaussian.  \code{rnig} uses this representation to
  generate observations from the normal inverse Gaussian distribution.
  Generalized inverse Gaussian observations are obtained via the algorithm of
  Dagpunar (1989).}

\value{
  \code{dnig} gives the density, \code{pnig} gives the distribution
  function, \code{qnig} gives the quantile function and \code{rnig}
  generates random variates. An estimate of the accuracy of the
  approximation to the distribution function may be found by setting
  \code{accuracy = TRUE} in the call to \code{pnig} which then returns
  a list with components \code{value} and \code{error}.

  \code{ddnig} gives the derivative of \code{dnig}.
 }

\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.

  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley

  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

\seealso{
  \code{\link[DistributionUtils]{safeIntegrate}},
  \code{\link{integrate}} for its shortfalls, \code{\link{splinefun}},
  \code{\link{uniroot}} and \code{\link{hyperbChangePars}} for changing
  parameters to the \eqn{(\alpha,\beta)}{(alpha, beta)}
  parameterization, \code{\link{dghyp}} for the generalized hyperbolic
  distribution.
}

\examples{
param <- c(0, 2, 1, 0)
nigRange <- nigCalcRange(param = param, tol = 10^(-3))
par(mfrow = c(1, 2))
curve(dnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Density of the\n Normal Inverse Gaussian Distribution")
curve(pnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Distribution Function of the\n Normal Inverse Gaussian Distribution")
dataVector <- rnig(500, param = param)
curve(dnig(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add =TRUE)
title("Density and Histogram\n of the Normal Inverse Gaussian Distribution")
DistributionUtils::logHist(dataVector, main =
        "Log-Density and Log-Histogram\n of the Normal Inverse Gaussian Distribution")
curve(log(dnig(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)
par(mfrow = c(2, 1))
curve(dnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Density of the\n Normal Inverse Gaussian Distribution")
curve(ddnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Derivative of the Density\n of the Normal Inverse Gaussian Distribution")
}

\keyword{distribution}
