\name{covEllipsesPlot}


\alias{covEllipsesPlot}


\title{Displays a Covariance Ellipses Plot}


\description{

    Displays a covariance ellipses plot.

}


\usage{
covEllipsesPlot(x = list(), \dots)
}


\arguments{

    \item{x}{
        a list of at least two covariance matrices.
        }
    \item{\dots}{
        optional arguments to be passed.\cr
        }

}


\details{

    This plot visualizes the difference between two or more covariance matrices.
    It is meant to compare different methods of covariance estimation.

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   head(LPP)
   
## cov - 
   Cov = cov(LPP)
   robustCov = assetsMeanCov(LPP, "MCD")$Sigma
   
## covEllipsesPlot -
   covEllipsesPlot(list(Cov, robustCov))  
}


\keyword{models}
