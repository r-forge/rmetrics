\name{UnitrootTests}
\alias{UnitrootTests}

\alias{unitrootTest}
\alias{adfTest}

\concept{unit root tests}
\concept{Dickey-Fuller test}
\concept{augmented Dickey-Fuller test}
\concept{MacKinnon's unit root test}


\title{Unit root time series tests}

\description{
  Carry out augmented Dickey-Fuller tests for unit roots based on
  Banerjee's et al. tables and on J.G. McKinnons' numerical distribution
  functions.
}

\usage{
unitrootTest(x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)
    
adfTest(x, lags = 1, type = c("nc", "c", "ct"), title = NULL, 
    description = NULL)
}

\arguments{
  \item{x}{
    a numeric vector or time series object.
  }  
  \item{lags}{
    the maximum number of lags used for error term correction.
  }
  \item{type}{    
    a character string describing the type of the unit root
    regression. Valid choices are \code{"nc"} for a regression with no
    intercept (constant) nor time trend, \code{"c"} for a regression
    with an intercept (constant) but no time trend, \code{"ct"} for a
    regression with an intercept (constant) and a time trend. The
    default is \code{"c"}.
  }
  \item{title}{
    a character string which allows for a project title.
  }
  \item{description}{
    a character string which allows for a brief description.
  }
}

\details{
    \code{adfTest()} computes test statistics and p-values along the
    implementation from Trapletti's augmented Dickey-Fuller test for
    unit roots. In contrast to Trapletti's function, three kind of test
    types can be selected.
    
    \code{unitrootTest()} computes test statistics and p-values using
    McKinnon's response surface approach.
}

\value{

    an object from S4 class \code{"fHTEST"} with the following slots:

    \item{@call}{
      the function call.      
    }
    \item{@data}{
      a data frame with the input data.
    }
    \item{@data.name}{
      a character string giving the name of the data frame.
    }
    \item{@test}{
      a list object which holds the output of the underlying test
      function.
    }
    \item{@title}{
      a character string with the name of the test.
    }
    \item{@description}{
      a character string with a brief description of the test.
    }
        
    The entries of the \code{@test} slot include the following components:

    \item{$statistic}{
      the value of the test statistic.
    }
    \item{$parameter}{
      the lag order.
    }
    \item{$p.value}{
      the p-value of the test.
    }
    \item{$method}{
      a character string indicating what type of test was performed.
    } 
    \item{$data.name}{
      a character string giving the name of the data.
    }
    \item{$alternative}{
      a character string describing the alternative hypothesis.
    }
    \item{$name}{
      the name of the underlying function, which may be wrapped.
    }
    \item{$output}{
      additional test results to be printed.
    }
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
    
MacKinnon, J.G. (1996);
    \emph{Numerical distribution functions for unit root and 
        cointegration tests},
    Journal of Applied Econometrics 11, 601--618.
    
Said S.E., Dickey D.A. (1984);
    \emph{Testing for Unit Roots in Autoregressive-Moving Average 
        Models of Unknown Order},
    Biometrika 71, 599--607.
}

\author{
  Adrian Trapletti for the tests adapted from R's "tseries" package, \cr
  Diethelm Wuertz for the Rmetrics \R-port.
}

\examples{
## a time series which contains no unit-root:
x <- rnorm(1000)  
## a time series which contains a unit-root:
y <- cumsum(c(0, x))
   
adfTest(x)
adfTest(y)
   
unitrootTest(x)
unitrootTest(y)     
}

\keyword{htest}
