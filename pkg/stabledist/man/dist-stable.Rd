\name{StableDistribution}

\title{Stable Distribution Function}

\alias{StableDistribution}
\alias{dstable}
\alias{pstable}
\alias{qstable}
\alias{rstable}

\concept{Stable Distribution}
\encoding{utf-8}

\description{
  Compute density, distribution and quantile function and
  to generate random variates of the stable distribution.

  The four functions are:
  \tabular{ll}{
    \code{[dpqr]stable} \tab the (skewed) stable distribution.
  }

  Three parametrizations via \code{pm = 0, 1,}  or \code{2} \emph{differ} in their
  meaning of \code{delta} and \code{gamma}, see \sQuote{Details} below.
  Notably the special cases of the Gaussian / normal distribution for \code{alpha = 2}
  and Cauchy distribution for \code{alpha = 1} and \code{beta = 0} are
  easily matched for \code{pm = 2}.
}

\usage{
dstable(x, alpha, beta, gamma = 1, delta = 0, pm = 0,
        log = FALSE,
        tol = 64*.Machine$double.eps, zeta.tol = NULL,
        subdivisions = 1000)
pstable(q, alpha, beta, gamma = 1, delta = 0, pm = 0,
        lower.tail = TRUE, log.p = FALSE, silent = FALSE,
        tol = 64*.Machine$double.eps, subdivisions = 1000)
qstable(p, alpha, beta, gamma = 1, delta = 0, pm = 0,
        lower.tail = TRUE, log.p = FALSE,
        tol = .Machine$double.eps^0.25, maxiter = 1000, trace = 0,
        integ.tol = 1e-7, subdivisions = 200)
rstable(n, alpha, beta, gamma = 1, delta = 0, pm = 0)
}

\arguments{
  \item{alpha, beta, gamma, delta}{
    value of the index parameter \code{alpha} in the interval= \eqn{(0, 2]};
    skewness parameter \code{beta}, in the range \eqn{[-1, 1]};
    scale parameter \code{gamma}; and location (or \sQuote{shift})
    parameter \code{delta}.
  }
  \item{n}{sample size (integer).}
  \item{p}{numeric vector of probabilities.}
  \item{pm}{
    parameterization, an integer in \code{0, 1, 2}; by default \code{pm=0},
    the \sQuote{S0} parameterization.}
  \item{x, q}{numeric vector of quantiles.}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
  \item{silent}{logical indicating that e.g., warnings should be
    suppressed when \code{NaN} is produced (because of numerical problems).}

  \item{integ.tol}{positive number, the tolerance used for numerical
    integration, see \code{\link{integrate}}.}
  \item{tol}{numerical tolerance,
    \describe{
      \item{dstable(), pstable():}{used for numerical integration, see
	\code{integ.tol} above.  Note that earlier versions had tighter
	tolerances -- which seem too tight as default values.}
      \item{qstable():}{used for rootfinding, see \code{\link{uniroot}}.}
    }
  }
  \item{zeta.tol}{(\code{dstable}) numerical tolerance for checking if
    \code{x} is close to \eqn{\zeta(\alpha,\beta)}.  The default,
    \code{NULL} depends itself on \eqn{(\alpha,\beta)}.
    \cr
    As it is \emph{experimental} and not guaranteed to remain in the
    future, its use is not recommended in production code.  Rather e-mail the
    package maintainer about it.}
  \item{subdivisions}{maximal number of intervals for integration, see
    \code{\link{integrate}}.}
  \item{maxiter, trace}{maximal number of iterations and verboseness in
    \code{\link{uniroot}}, see there.}
}

\value{
    All values for the \code{*stable} functions
    are numeric vectors:
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
}

\details{
    \bold{Skew Stable Distribution:}
    \cr\cr
    The function uses the approach of J.P. Nolan for general stable
    distributions.  Nolan (1997) derived expressions in form of integrals
    based on the characteristic function for standardized stable random
    variables.  For \code{dstable} and \code{pstable}, these integrals
    are numerically evaluated using \R's \code{\link{integrate}()}
    function.
    \cr
    \dQuote{S0} parameterization [pm=0]: based on the (M) representation
    of Zolotarev for an alpha stable distribution with skewness
    beta.  Unlike the Zolotarev (M) parameterization, gamma and
    delta are straightforward scale and shift parameters.  This
    representation is continuous in all 4 parameters, and gives
    an intuitive meaning to gamma and delta that is lacking in
    other parameterizations.
    \cr
    Switching the sign of \code{beta} \emph{mirrors} the distribution at
    the vertical axis \eqn{x = \delta}{x = delta}, i.e.,
    \deqn{f(x, \alpha, -\beta, \gamma, \delta, 0) =
      f(2\delta-x, \alpha, +\beta, \gamma, \delta, 0),}
    see the graphical example below.

    \dQuote{S} or \dQuote{S1} parameterization [pm=1]: the parameterization used
    by Samorodnitsky and Taqqu in the book Stable Non-Gaussian
    Random Processes.  It is a slight modification of Zolotarev's
    (A) parameterization.
    \cr
    \dQuote{S*} or \dQuote{S2} parameterization [pm=2]: a modification of the S0
    parameterization which is defined so that (i) the scale gamma
    agrees with the Gaussian scale (standard dev.) when alpha=2
    and the Cauchy scale when alpha=1, (ii) the mode is exactly at
    delta.  For this parametrization,
    \code{\link{stableMode}(alpha,beta)} is needed.
    \cr
    \dQuote{S3} parameterization [pm=3]: an internal parameterization,
    currently not available for these functions.  The scale is the same
    as the \dQuote{S2} parameterization, the shift is
    \eqn{-\beta*g(\alpha)}, where \eqn{g(\alpha)} is defined in Nolan(1999).
}
\section{Tail Behavior}{
  The asymptotic behavior for large \eqn{x}, aka \dQuote{tail behavior}
  for the cumulative \eqn{F(x) = P(X \le x)}{F(x) = P(X <= x)}
  is (for \eqn{x\to\infty}{x -> Inf})
  \deqn{1 - F(x) \sim (1+\beta) C_\alpha x^{-\alpha},}{1 - F(x) ~ (1+b) C_a x^-a,}
  where \eqn{C_\alpha = \Gamma(\alpha)/\pi \sin(\alpha\pi/2)
           }{a=alpha, b=beta, C_a = Gamma(a)/pi * sin(a*pi/2)}; hence also
  \deqn{F(-x) \sim (1-\beta) C_\alpha x^{-\alpha}.}{F(-x) ~ (1+b) C_a x^-a.}

  Differentiating \eqn{F()} above gives
  \deqn{f(x) \sim \alpha(1+\beta) C_\alpha x^{-(1+\alpha)}.}{f(x) ~ a(1+b) C_a x^-(1+a).}
}
\note{
  In the case \eqn{\beta = 1}, the distributions are \dQuote{maximally
    skewed to the right} or simply \dQuote{\emph{extremal} stable}
  (Zolotarev).  In that case, the package \pkg{FMStable} provides
  \code{dpq*} functions which are faster and more accurate than ours (if
  accuracy higher than about 6 digits is needed), see,
  \code{\link[FMStable]{pEstable}}.

  When \code{alpha} is close to 1 or close to 0 (\dQuote{close},
  e.g., meaning distance \eqn{d < 0.01}), the computations typically are
  numerically considerably more challenging, and the results may not be
  accurate.
  \cr
  As we % currently (2011-03-28)
  plan to improve on this, \emph{and}
  as it is unknown when exactly the numerical difficulties arise, we
  mainly only warn here in the documentation, and only in some cases,
  e.g. when the root finding with \code{\link{uniroot}} fails,
  signal explicit \code{\link{warning}()}s and may return \code{NaN} then.
}
\seealso{
  the \code{\link[fBasics]{stableSlider}()} function from package
  \pkg{fBasics} for displaying densities and probabilities of these
  distributions, for educational purposes.

  Royuela del Val et al. (2017) partly show to be uniformly
  better both accuracy and speed wise than our computations;
  While their package \pkg{libstableR} is no longer on CRAN, there is
  \CRANpkg{libstable4u} derived from their implementation.
}
\author{
  Diethelm Wuertz for the original Rmetrics \R-port.
  Many numerical improvements by Martin Maechler.
}

\references{
  Chambers J.M., Mallows, C.L. and Stuck, B.W. (1976)
  \emph{A Method for Simulating Stable Random Variables},
  J. Amer. Statist. Assoc. \bold{71}, 340--344.

  John P. Nolan (2020)
  \emph{Univariate Stable Distributions - Models for Heavy Tailed Data}
  Springer Series in Operations Research and Financial Engineering;
  \doi{10.1007/978-3-030-52915-4}
  Much earlier version of chapter 1 available at
  \url{https://edspace.american.edu/jpnolan/stable/},
  see  \dQuote{Introduction to Stable Distributions}

  Nolan J.P. (1997)
  Numerical calculation of stable densities and distribution functions.
  \emph{Stochastic Models} \bold{13}(4), 759--774.
  \cr
  Also available as \file{density.ps} from Nolan's web page.

  Samoridnitsky G., Taqqu M.S. (1994);
    \emph{Stable Non-Gaussian Random Processes, Stochastic Models
    with Infinite Variance},
    Chapman and Hall, New York, 632 pages.

  Weron, A., Weron R. (1999);
    \emph{Computer Simulation of Levy alpha-Stable Variables and
    Processes},
    Preprint Technical University of Wroclaw, 13 pages.

  Royuela-del-Val, J., Simmross-Wattenberg, F., and Alberola-López, C. (2017)
    libstable: Fast, Parallel, and High-Precision Computation of \eqn{\alpha}-Stable
    Distributions in R, C/C++, and MATLAB.
    \emph{Journal of Statistical Software} \bold{78}(1), 1--25.
    \doi{doi:10.18637/jss.v078.i01}
}

\examples{
## stable -

## Plot stable random number series
   set.seed(1953)
   r <- rstable(n = 1000, alpha = 1.9, beta = 0.3)
   plot(r, type = "l", main = "stable: alpha=1.9 beta=0.3",
        col = "steelblue")
   grid()

## Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white",
        col = "steelblue")
   x <- seq(-5, 5, by=1/16)
   lines(x, dstable(x, alpha = 1.9, beta = 0.3, tol= 1e-3), lwd = 2)

## Plot df and compare with true df:
   plot(ecdf(r), do.points=TRUE, col = "steelblue",
        main = "Probabilities:  ecdf(rstable(1000,..)) and true  cdf F()")
   rug(r)
   lines(x, pstable(q = x, alpha = 1.9, beta = 0.3),
         col="#0000FF88", lwd= 2.5)

## Switching  sign(beta)  <==> Mirror the distribution around  x == delta:
curve(dstable(x, alpha=1.2, beta =  .8, gamma = 3, delta = 2), -10, 10)
curve(dstable(x, alpha=1.2, beta = -.8, gamma = 3, delta = 2),
      add=TRUE, col=2)
## or the same
curve(dstable(2*2-x, alpha=1.2, beta = +.8, gamma = 3, delta = 2),
      add=TRUE, col=adjustcolor("gray",0.2), lwd=5)
abline(v = 2, col = "gray", lty=2, lwd=2)
axis(1, at = 2, label = expression(delta == 2))

## Compute quantiles:
   x. <- -4:4
   px <- pstable(x., alpha = 1.9, beta = 0.3)
  (qs <- qstable(px, alpha = 1.9, beta = 0.3))
stopifnot(all.equal(as.vector(qs), x., tol = 1e-5))

## Special cases: --- 1. Gaussian  alpha = 2 -----------
x. <- seq(-5,5, by=1/16)
stopifnot(
    all.equal(
        pnorm  (x.,     m=pi,    sd=1/8),
        pstable(x., delta=pi, gamma=1/8, alpha = 2, beta = 0, pm = 2) )
   ,
##                --- 2. Cauchy  alpha = 1 -----------
    all.equal(
        pcauchy(x.),
        pstable(x., delta=0, gamma=1, alpha = 1, beta = 0, pm = 2) )
)
}

\keyword{distribution}
