\name{nigFitStart}
\alias{nigFitStart}
\alias{nigFitStartMoM}

\title{Find Starting Values for Fitting a normal inverse Gaussian Distribution}
\description{
  Finds starting values for input to a maximum likelihood routine for
  fitting normal inverse Gaussian distribution to data.
}

\usage{
  nigFitStart(x, startValues = c("FN","Cauchy","MoM","US"),
              paramStart = NULL,
              startMethodMoM = c("Nelder-Mead","BFGS"), ...)
  nigFitStartMoM(x, startMethodMoM = "Nelder-Mead", ...)
}

\arguments{
  \item{x}{data vector.}
  \item{startValues}{a \code{\link{character}} strin specifying the
    method for starting values to consider.  See \bold{Details}.}
  \item{paramStart}{starting values for param if \code{startValues = "US"}.}
  \item{startMethodMoM}{Method used by call to \code{\link{optim}} in
    finding method of moments estimates.}
  \item{\dots}{Passes arguments to \code{\link{hist}} and
    \code{\link{optim}}.}
}

\details{
  Possible values of the argument \code{startValues} are the following:
  \describe{
    \item{\code{"US"}}{User-supplied.}
    \item{\code{"FN"}}{A fitted normal distribution.}
    \item{\code{"Cauchy"}}{Based on a fitted Cauchy distribution, from
      \code{\link[MASS]{fitdistr}()} of the \CRANpkg{MASS} package.}
    \item{\code{"MoM"}}{Method of moments.}
  }
  If \code{startValues = "US"} then a value must be supplied for
  \code{paramStart}.

  If \code{startValues = "MoM"}, \code{nigFitStartMoM} is
  called. If \code{startValues = "MoM"} an initial
  optimisation is needed to find the starting values. These
  optimisations call \code{\link{optim}}.
}


\value{
  \code{nigFitStart} returns a list with components:
    \item{paramStart}{A vector with elements \code{mu}, \code{delta},
      \code{alpha} and \code{beta} giving the
      starting value of param.}
    \item{xName}{A character string with the actual \code{x} argument name.}
    \item{breaks}{The cell boundaries found by a call to
      \code{\link{hist}}.}
    \item{midpoints}{The cell midpoints found by a call to
      \code{\link{hist}}.}
    \item{empDens}{The estimated density found by a call to
      \code{\link{hist}}.}

  \code{nigFitStartMoM} returns only the method of moments estimates
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

\author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

\seealso{
  \code{\link{dnig}}, \code{\link{dskewlap}},
  \code{\link{nigFit}}, \code{\link{hist}}, \code{\link{optim}},
  \code{\link[MASS]{fitdistr}}.
}

\examples{
param <- c(2, 2, 2, 1)
dataVector <- rnig(500, param = param)
nigFitStart(dataVector, startValues = "FN")
nigFitStartMoM(dataVector)
nigFitStart(dataVector, startValues = "MoM")
}

\keyword{distribution}
