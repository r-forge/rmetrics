\name{hyperbFitStart}
\alias{hyperbFitStart}
\alias{hyperbFitStartMoM}

\title{Find Starting Values for Fitting a Hyperbolic Distribution}
\description{
  Finds starting values for input to a maximum likelihood routine for
  fitting hyperbolic distribution to data.
}

\usage{
  hyperbFitStart(x, startValues = c("BN","US","FN","SL","MoM"),
                 paramStart = NULL,
                 startMethodSL = c("Nelder-Mead","BFGS"),
                 startMethodMoM = c("Nelder-Mead","BFGS"), ...)
  hyperbFitStartMoM(x, startMethodMoM = "Nelder-Mead", ...)
}

\arguments{
  \item{x}{Data vector.}
  \item{startValues}{Vector of the different starting values to consider.
    See \bold{Details}.}
  \item{paramStart}{Starting values for param if \code{startValues =
      "US"}.}
  \item{startMethodSL}{Method used by call to \code{\link{optim}} in
    finding skew Laplace estimates.}
  \item{startMethodMoM}{Method used by call to \code{\link{optim}} in
    finding method of moments estimates.}
  \item{\dots}{Passes arguments to \code{\link{hist}} and
    \code{\link{optim}}.}
}

\details{
  Possible values of the argument \code{startValues} are the following:
  \describe{
    \item{\code{"US"}}{User-supplied.}
    \item{\code{"BN"}}{Based on Barndorff-Nielsen (1977).}
    \item{\code{"FN"}}{A fitted normal distribution.}
    \item{\code{"SL"}}{Based on a fitted skew-Laplace distribution.}
    \item{\code{"MoM"}}{Method of moments.}
  }
  If \code{startValues = "US"} then a value must be supplied for
  \code{paramStart}.

  If \code{startValues = "MoM"}, \code{hyperbFitStartMoM} is
  called. These starting values are based on Barndorff-Nielsen \emph{et
  al} (1985).

  If \code{startValues = "SL"}, or \code{startValues = "MoM"} an initial
  optimisation is needed to find the starting values. These
  optimisations call \code{\link{optim}}.
}


\value{
  \code{hyperbFitStart} returns a list with components:
    \item{paramStart}{A vector with elements \code{mu}, \code{delta},
      \code{alpha} and \code{beta} giving the
      starting value of param.}
    \item{breaks}{The cell boundaries found by a call to
      \code{\link{hist}}.}
    \item{midpoints}{The cell midpoints found by a call to
      \code{\link{hist}}.}
    \item{empDens}{The estimated density found by a call to
      \code{\link{hist}}.}

  \code{hyperbFitStartMoM} returns only the method of moments estimates
  as a vector with elements \code{mu}, \code{delta}, \code{alpha} and
  \code{beta}.
}

\references{
  Barndorff-Nielsen, O. (1977)
  Exponentially decreasing distributions for the logarithm of particle size,
  \emph{Proc. Roy. Soc. Lond.},
  A\bold{353}, 401--419.

  Barndorff-Nielsen, O., \enc{Blæsild}{Blaesild}, P., Jensen, J.,
  and  \enc{Sörenson}{Sorenson}, M. (1985).
  The fascination of sand.
  In \emph{A celebration of statistics, The ISI Centenary Volume},
  eds., Atkinson, A. C. and Fienberg, S. E.,
  pp. 57--87. New York: Springer-Verlag.

  Fieller, N. J., Flenley, E. C. and Olbricht, W. (1992)
  Statistics of particle size data.
  \emph{Appl. Statist.},
  \bold{41}, 127--146.

}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Ai-Wei Lee, Jennifer Tso, Richard Trendall, Thomas Tran}


\seealso{
  \code{\link{dhyperb}}, \code{\link{dskewlap}},
  \code{\link{hyperbFit}}, \code{\link{hist}}, and
  \code{\link{optim}}.
}

\examples{
param <- c(2, 2, 2, 1)
dataVector <- rhyperb(500, param = param)
hyperbFitStart(dataVector, startValues = "FN")
hyperbFitStartMoM(dataVector)
hyperbFitStart(dataVector, startValues = "MoM")
}

\keyword{distribution}
