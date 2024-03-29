\name{nigFit}
\alias{nigFit}


\title{Fit of a Normal Inverse Gaussian Distribution}

\description{

  Estimates the parameters of a normal inverse Gaussian distribution.

}

\usage{   
nigFit(x, alpha = 1, beta = 0, delta = 1, mu = 0, 
    method = c("mle", "gmm", "mps", "vmps"), scale = TRUE, doplot = TRUE, 
    span = "auto", trace = TRUE, title = NULL, description = NULL, \dots) 
}

\arguments{
  
  \item{alpha, beta, delta, mu}{
      The parameters are \code{alpha}, \code{beta}, \code{delta}, and
      \code{mu}:\cr
      shape parameter \code{alpha};
      skewness parameter \code{beta}, \code{abs(beta)} is in the 
      range (0, alpha);
      scale parameter \code{delta}, \code{delta} must be zero or 
      positive; 
      location parameter \code{mu}, by default 0.
      These is the meaning of the parameters in the first 
      parameterization \code{pm=1} which is the default 
      parameterization selection.
      In the second parameterization, \code{pm=2} \code{alpha}
      and \code{beta} take the meaning of the shape parameters
      (usually named) \code{zeta} and \code{rho}.
      In the third parameterization, \code{pm=3} \code{alpha}
      and \code{beta} take the meaning of the shape parameters
      (usually named) \code{xi} and \code{chi}.
      In the fourth parameterization, \code{pm=4} \code{alpha}
      and \code{beta} take the meaning of the shape parameters
      (usually named) \code{a.bar} and \code{b.bar}.
      }
  \item{description}{
      a character string which allows for a brief description.
      }
  \item{doplot}{
      a logical flag. Should a plot be displayed?
      }
  \item{method}{
      a character string. Either 
      \code{"mle"}, Maximum Likelihood Estimation, the default, 
      \code{"gmm"} Gemeralized Method of Moments Estimation, 
      \code{"mps"} Maximum Product Spacings Estimation, or
      \code{"vmps"} Minimum Variance Product Spacings Estimation.
      }
  \item{scale}{
      a logical flag, by default \code{TRUE}. Should the time series
      be scaled by its standard deviation to achieve a more stable
      optimization?
      }
  \item{span}{
      x-coordinates for the plot, by default 100 values 
      automatically selected and ranging between the 0.001, 
      and 0.999 quantiles. Alternatively, you can specify
      the range by an expression like \code{span=seq(min, max,
      times = n)}, where, \code{min} and \code{max} are the 
      left and right endpoints of the range, and \code{n} gives 
      the number of the intermediate points.
      }
  \item{title}{
      a character string which allows for a project title.
      }
  \item{trace}{
      a logical flag. Should the parameter estimation process be
      traced?
      }
  \item{x}{
      a numeric vector. 
      }
  \item{\dots}{
      parameters to be parsed.
      }

}

\value{
  an object from class \code{"fDISTFIT"}.

  Slot \code{fit} is a list with the following components:
  
    \item{estimate}{
        the point at which the maximum value of the log liklihood 
        function is obtained.
        }
    \item{minimum}{
        the value of the estimated maximum, i.e. the value of the
        log liklihood function.
        }
    \item{code}{
        an integer indicating why the optimization process terminated.\cr
        1: relative gradient is close to zero, current iterate is probably 
            solution; \cr
        2: successive iterates within tolerance, current iterate is probably 
            solution; \cr
        3: last global step failed to locate a point lower than \code{estimate}. 
            Either \code{estimate} is an approximate local minimum of the 
            function or \code{steptol} is too small; \cr
        4: iteration limit exceeded; \cr
        5: maximum step size \code{stepmax} exceeded five consecutive times. 
            Either the function is unbounded below, becomes asymptotic to a 
            finite value from above in some direction or \code{stepmax} 
            is too small.
            }
    \item{gradient}{
        the gradient at the estimated maximum.
        }
    \item{steps}{
        number of function calls.
        }
              
}

\examples{    
## nigFit -
   # Simulate Random Variates:
   set.seed(1953)
   s = rnig(n = 1000, alpha = 1.5, beta = 0.3, delta = 0.5, mu = -1.0) 

## nigFit -  
   # Fit Parameters:
   nigFit(s, alpha = 1, beta = 0, delta = 1, mu = mean(s), doplot = TRUE) 
}

\keyword{distribution}
