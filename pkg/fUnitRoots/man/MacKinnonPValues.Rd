\name{MacKinnonPValues}

\alias{MacKinnonPValues}

\alias{punitroot}
\alias{qunitroot}
\alias{unitrootTable}

\concept{unit root tests}
\concept{distribution of MacKinnon's test statistic}


\title{MacKinnon's Unit Root p Values}

\description{
    A collection and description of functions 
    to compute the distribution and quantile 
    function for MacKinnon's unit root test statistics. 
}

\usage{
punitroot(q, N = Inf, trend = c("c", "nc", "ct", "ctt"), 
    statistic = c("t", "n"), na.rm = FALSE)
qunitroot(p, N = Inf, trend = c("c", "nc", "ct", "ctt"), 
    statistic = c("t", "n"), na.rm = FALSE)
    
unitrootTable(trend = c("c", "nc", "ct", "ctt"), statistic = c("t", "n")) 
}

\arguments{ 
  \item{q}{   
    vector of quantiles or test statistics. Missing values are allowed.
  }
  \item{p}{ 
    a numeric vector of probabilities. Missing values are allowed.
  }
  \item{N}{ 
    the number of observations in the sample from which the quantiles
    are to be computed.
  }
  \item{na.rm}{
    a logical value. If set to \code{TRUE}, missing values will be
    removed, otherwise not. The default is \code{FALSE}.
  }
  \item{statistic}{ 
    a character string describing the type of test statistic.  Valid
    choices are \code{"t"} for t-statistic, and \code{"n"} for
    normalized statistic, sometimes referred to as the
    rho-statistic. The default is \code{"t"}.
  }
  \item{trend}{
    a character string describing the regression from which the
    quantiles are to be computed. Valid choices are: \code{"nc"} for a
    regression with no intercept (constant) nor time trend, \code{"c"}
    for a regression with an intercept (constant) but no time trend, and
    \code{"ct"} for a regression with an intercept (constant) and a time
    trend. The default is \code{"c"}.
  }
}

\details{

  \code{punitroot} computes the cumulative probability of the asymptotic
  or finite sample distribution of the unit root test statistics.
    
  \code{qunitroot} computes the quantiles of the asymptotic or finite
  sample distribution of the unit root test statistics, given the
  probabilities.

  \code{unitrootTable} produces tables of p-values from MacKinnon's
  response surface.
}

\value{
  for \code{punitroot} and \code{qunitroot}, a numeric vector,

  for \code{unitrootTable}, a matrix with attribute \code{"control"}
  containing information about the type of test.
}

\note{
  The function \code{punitroot} and \code{qunitroot} use Fortran 
  routines and the response surface approach from J.G. MacKinnon (1988). 
  Many thanks to J.G. MacKinnon putting his code and tables under the 
  GPL license, which made this implementation possible.
}

\author{
  J.G. MacKinnon for the underlying Fortran routine and the tables, \cr
  Diethelm Wuertz for the Rmetrics \R-port.
}
    
\references{ 
Dickey, D.A., Fuller, W.A. (1979);
    \emph{Distribution of the estimators for autoregressive time 
        series with a unit root}, 
    Journal of the American Statistical Association 74, 427--431. 

MacKinnon, J.G. (1996);
    \emph{Numerical distribution functions for unit root and 
        cointegration tests},
    Journal of Applied Econometrics 11, 601--618.

Phillips, P.C.B., Perron, P. (1988);
    \emph{Testing for a unit root in time series regression}, 
    Biometrika 75, 335--346.
}

\examples{ 
## Asymptotic quantile of t-statistic
qunitroot(0.95, trend = "nc", statistic = "t")
## 1st argument a vector
qunitroot(c(0.90, 0.95), trend = "nc", statistic = "t")

## Finite sample quantile of n-statistic
qunitroot(0.95, N = 100, trend = "nc", statistic = "n") 


## Asymptotic cumulative probability of t-statistic
punitroot(1.2836, trend = "nc", statistic = "t")

## Finite sample cumulative probability of n-statistic
punitroot(1.2836, N = 100, trend = "nc", statistic = "n")

   
## Mac Kinnon's unitrootTable
unitrootTable(trend = "nc")
}

\keyword{distribution}
