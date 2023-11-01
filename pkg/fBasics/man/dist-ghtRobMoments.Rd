\name{ghtRobMoments}
\alias{ghtRobMoments}

\alias{ghtMED}
\alias{ghtIQR}
\alias{ghtSKEW}
\alias{ghtKURT}


\title{Robust Moments for the GHT}

\description{

  Computes the first four robust moments for the
  generalized hyperbolic Student-t.

}

\usage{
ghtMED(beta = 0.1, delta = 1, mu = 0, nu = 10)
ghtIQR(beta = 0.1, delta = 1, mu = 0, nu = 10)
ghtSKEW(beta = 0.1, delta = 1, mu = 0, nu = 10)
ghtKURT(beta = 0.1, delta = 1, mu = 0, nu = 10)
}

\arguments{
    \item{beta}{skewness parameter in the range \code{(0, alpha)}.}
    \item{delta}{scale parameter, must be zero or positive.}
    \item{mu}{location parameter, by default 0.}
    \item{nu}{
        a numeric value, the number of degrees of freedom.
        Note, \code{alpha} takes the limit of \code{abs(beta)},
        and \code{lambda=-nu/2}.
        }
}

\details{

  The parameters are those of the first parameterization.

}

\value{

    All values for the \code{*ght} functions are numeric vectors:
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.

    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.

}

\author{
  Diethelm Wuertz.
}

\examples{
## ghtMED -
   # Median:
   ghtMED(beta = 0.1, delta = 1, mu = 0, nu = 10)

## ghtIQR -
   # Inter-quartile Range:
   ghtIQR(beta = 0.1, delta = 1, mu = 0, nu = 10)

## ghtSKEW -
   # Robust Skewness:
   ghtSKEW(beta = 0.1, delta = 1, mu = 0, nu = 10)

## ghtKURT -
   # Robust Kurtosis:
   ghtKURT(beta = 0.1, delta = 1, mu = 0, nu = 10)

}

\keyword{distribution}
