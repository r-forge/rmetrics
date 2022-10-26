\name{show-methods}
\alias{show-methods}

\docType{methods}

\alias{show,ANY-method}
\alias{show,fGARCH-method}
\alias{show,fGARCHSPEC-method}


\title{GARCH modelling show methods}
\description{

  Show methods for GARCH Modelling, i.e., methods for the
  \code{\link{show}()} generic function (which typically and also here
  are also used as \emph{the} \code{\link{print}} methods for S4
  objects.

}

\author{
  Diethelm Wuertz for the Rmetrics \R-port
}

\section{Methods}{
  Methods for \code{show} defined in package \pkg{fGarch}:
  \describe{
    \item{object = "fGARCH"}{
      Print function for objects of class \code{"fGARCH"}.
    }
    \item{object = "fGARCHSPEC"}{
      Print function for objects of class \code{"fGARCH"}.
    }
  }
}

\examples{
## garchSpec -
   spec = garchSpec()
   spec # print() or show() it

## garchSim -
   x = garchSim(spec, n = 500)

## garchFit -
   fit = garchFit(~ garch(1, 1), data = x)
   fit # == print(fit) and also == show(fit)
}
\keyword{methods}
