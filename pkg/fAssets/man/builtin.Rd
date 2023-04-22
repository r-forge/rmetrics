\name{builtin}
    
\alias{.baggedMeanCov}
\alias{.bayesSteinMeanCov}
\alias{.cov.arw}
\alias{.cov.nnve}
\alias{.cov.shrink}
\alias{.donostahMeanCov}
\alias{.ledoitWolfMeanCov}
\alias{.rmtMeanCov}
\alias{.studentMeanCov}

\title{Estimation of Mean and Covariances of Asset Sets}


\description{
  
    Helper functions for estimating the mean and/or covariance matrix of a 
    time series of assets by traditional and robust methods.
    
}
    

\usage{

.baggedMeanCov(x, baggedR = 100, ...)
.bayesSteinMeanCov(x, ...)
.cov.arw(x, center, cov, alpha = 0.025, pcrit = NULL)
.cov.nnve(datamat, k = 12, pnoise = 0.05, emconv = 0.001, bound = 1.5,
extension = TRUE, devsm = 0.01)
.cov.shrink(x, lambda, verbose = FALSE)
.donostahMeanCov(x, ...)
.ledoitWolfMeanCov(x, ...)
.rmtMeanCov(x, ...)
.studentMeanCov(x, ...)

}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
      }
    \item{baggedR}{
        when \code{methode="bagged"}, an integer value, the number of 
        bootstrap replicates, by default 100.
      }
    \item{center}{
	specifies for a data set (n x p), the initial location
        estimator(1 x p).
      }
    \item{cov}{
      Initial scatter estimator (p x p).
      }
    \item{alpha}{
	Maximum thresholding proportion (optional scalar, default:
        \code{alpha = 0.025}).
      }
    \item{pcrit}{
	critical value for outlier probability (optional scalar, default
        values from simulations).
      }
    \item{datamat}{
	a matrix in which each row represents an observation or
	point and each column represents a variable.
      }
    \item{k}{
	desired number of nearest neighbors (default is 12).
      }
    \item{pnoise}{
	percent of added noise
      }
    \item{emconv}{
	convergence tolerance for EM.
      }
    \item{bound}{
	value used to identify surges in variance caused by outliers
	wrongly included as signal points (bound = 1.5 means a 50
	percent increase).
      }
    \item{extension}{
	whether or not to continue after reaching the last chi-square
	distance. The default is to continue, which is indicated by
	setting \code{extension= TRUE}.
      }
    \item{devsm}{
	when \code{extension = TRUE}, the algorithm stops if the
	relative difference in variance is less than devsm (default is
	0.01).
      }
    \item{lambda}{
        the correlation shrinkage intensity (range 0-1). If lambda is
        not specified (the default) it is estimated using an analytic
        formula from Schaefer and Strimmer (2005) - see details
        below. For \code{lambda=0} the empirical correlations are
        recovered.
      }
    \item{verbose}{
	a logical indicating whether to print progress
        information to the stdout.
      }
    \item{\dots}{
        optional arguments to be passed to the underlying estimators. 
        For details we refer to the manual pages of the functions 
        \code{cov.rob} in the R package \code{MASS}, to the functions
        \code{covMcd} and \code{covOGK} in the R package
        \code{robustbase}.
        }     
}


\value{
    
    The functions return a list with elements containing the covariance
    and mean. The list may contain additional control parameters.
   
}


