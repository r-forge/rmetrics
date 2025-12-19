\name{DickeyFullerPValues}
\alias{DickeyFullerPValues}

\alias{padf}
\alias{qadf}
\alias{adfTable}

\concept{unit root tests}
\concept{distribution of Dickey-Fuller test statistic}
\concept{distribution of augmented Dickey-Fuller test statistic}


\title{Dickey-Fuller p Values}

\description{
    
  A collection and description of functions to compute the distribution
  and quantile function for the ADF unit root test statistics.
    
}

\usage{
padf(q, N = Inf, trend = c("nc", "c", "ct"), statistic = c("t", "n")) 
qadf(p, N = Inf, trend = c("nc", "c", "ct"), statistic = c("t", "n"))

adfTable(trend = c("nc", "c", "ct"), statistic = c("t", "n"), 
    includeInf = TRUE)
}

\arguments{ 
  \item{q}{   
    vector of quantiles or test statistics. Missing values are allowed.
  }
  \item{p}{ 
    vector of probabilities. Missing values are allowed.
  }
  \item{N}{ 
    the number of observations in the sample from which the quantiles
    are to be computed.
  }
  \item{trend}{
    a character string describing the regression from which the
    quantiles are to be computed. Valid choices are: \code{"nc"} for a
    regression with no intercept (constant) nor time trend, \code{"c"}
    for a regression with an intercept (constant) but no time trend, and
    \code{"ct"} for a regression with an intercept (constant) and a time
    trend. The default is \code{"c"}.
  }
  \item{statistic}{ 
    a character string describing the type of test statistic.  Valid
    choices are \code{"t"} for t-statistic and \code{"n"} for normalized
    statistic, sometimes referred to as the rho-statistic. The default
    is \code{"t"}.
  }
  \item{includeInf}{
    a logical flag. Should the asymptotic value be included into the
    table?
  }
}

\details{
  \code{padf} computes cumulative probabilities for the ADF test.
  
  \code{qadf} computes quantiles for the ADF test.

  With sufficiently fine grid for the first argument, \code{padf} and
  \code{qadf} compute, respectively, cumulative distribution functions
  and quantile functions of ADF test statistics.
  
  \code{adfTable}  produces tables of p-values for ADF tests.
}

\value{ 
  for \code{padf} and \code{qadf}, a named numeric vector with attribute
  \code{"control"} holding \code{N},

  for \code{adfTable}, an object from class \code{"gridData"}, which is
  a list with the following components:

  \item{x}{the values of \code{N} (length of the time series) for which
    the statistics are provided,}
  \item{y}{quantiles for which the statistics are provided,}
  \item{Table}{ a matrix with one row for each \code{N} in \code{x} and
    one column for each quantile in \code{y}.}

  The \code{"gridData"} object contains also attribute \code{"control"}
  with information about the requested test.
}

\note{
  \code{padf} and \code{qadf} is based on the tables from A. Banerjee et
  al. (1993). Interpolation is used For value of \code{N} not in the
  tables. For small \code{N} (\code{N < 25}) the result is \code{NA}.
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port.
}
    
\references{ 

Banerjee A., Dolado J.J., Galbraith J.W., Hendry D.F. (1993);
    \emph{Cointegration, Error Correction, and the Econometric 
        Analysis of Non-Stationary Data},
    Oxford University Press, Oxford. 
    
Dickey, D.A., Fuller, W.A. (1979);
    \emph{Distribution of the estimators for autoregressive time 
        series with a unit root}, 
    Journal of the American Statistical Association 74, 427--431. 
}

\examples{
padf(q = -2:2, N = 25)
qadf(p = (1:9)/10, N = 25)
adfTable()
}

\keyword{distribution}
