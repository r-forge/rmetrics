\name{snormSlider}
\alias{snormSlider}


\title{Skew normal distribution slider}

\description{
    
  Displays interactively the dependence of the skew Normal distribution
  on its parameters.
          
}


\usage{
snormSlider(type = c("dist", "rand"))
}

\arguments{

  \item{type}{

    a character string denoting which interactive plot should be
    displayed. Either a distribution plot \code{type="dist"}, the
    default value, or a random variates plot, \code{type="rand"}.

  }
}


\value{
  a Tcl object
}


\references{
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{
  Diethelm Wuertz for the Rmetrics \R-port
}

\seealso{
  \code{\link{snormFit}} (fit),
  \code{\link{snorm}},
}

\examples{
\dontrun{  
## snormSlider -
   require(tcltk)
   snormSlider("dist")
   snormSlider("rand")
}
}


\keyword{distribution}

