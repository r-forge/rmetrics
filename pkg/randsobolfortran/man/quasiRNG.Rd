\name{randsobolfortran}
\alias{randsobolfortran}
\alias{sobol.fortran}
\title{Toolbox for quasi random number generation}
\description{
	the Sobol sequence.
}
\usage{
sobol.fortran(n, dim = 1, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                   mixed = FALSE, method = "Fortran", mexp = 19937, start = 1,
                   maxit = 10)
}
\arguments{
  \item{n}{number of observations. If length(n) > 1, 
  	the length is taken to be the required number.}
  \item{dim}{dimension of observations default 1.}
  \item{init}{a logical, if \code{TRUE} the sequence is initialized and 
	restarts to the \code{start} value, otherwise not. By default \code{TRUE}.}
  \item{normal}{a logical if normal deviates are needed, default \code{FALSE}.}
  \item{scrambling}{
	    an integer value, if 1, 2 or 3 the sequence is scrambled
       otherwise not. If \code{scrambling=1}, Owen type type of scrambling is 
       applied, if \code{scrambling=2}, Faure-Tezuka type of scrambling, is
       applied, and if \code{scrambling=3}, both Owen+Faure-Tezuka type of
       scrambling is applied. By default 0.}
  \item{seed}{
        an integer value, the random seed for initialization
        of the scrambling process (only for \code{sobol}
        with \code{scrambling>0}). 
        If \code{NULL}, set to \code{4711}. }
  \item{mixed}{a logical to combine the QMC algorithm with the SFMT algorithm, default \code{FALSE}.}
  
  \item{method}{a character string either \code{"Fortran"}.}		 
  \item{mexp}{an integer for the Mersenne exponent of SFMT algorithm,
      only used when \code{mixed=TRUE}.}    
  \item{start}{an integer to initiliaze the sequence, default to 1,
      only used when \code{init=TRUE}.}
  \item{maxit}{a positive integer used to control inner loops both
      for generating randomized seed and for controlling
      outputs (when needed).}    
}
\details{

The currently available generator are given below.
Whatever the sequence, when \code{normal=TRUE}, outputs are transformed with
the quantile of the standard normal distribution \code{\link{qnorm}}.
If \code{init=TRUE}, the default, unscrambled and unmixed-SFMT quasi-random 
sequences start from \code{start}.
If \code{start != 0} and \code{normal=FALSE}, 
we suggest to use 0 as recommended by Owen (2020). 
One must handle the starting value (0) correctly if a quantile
function of a not-lower-bounded distribution is used.

\describe{
    

    \item{\bold{(scrambled) Sobol sequences}}{
	Computes uniform Sobol low discrepancy numbers. 
	The sequence starts from \eqn{k=1} when initialized with 
	\code{init = TRUE} (default).
	When \code{scrambling > 0}, a scrambling is performed
	or when \code{mixed = TRUE}, a randomized seed is performed.
  If some number of Sobol sequences are generated outside [0,1) with scrambling,
  the seed is randomized until we obtain all numbers in [0,1).
  One version of Sobol sequences is available the current version in 
	Fortran (\code{method = "Fortran"}) since \code{method = "C"} is under development.
	
	}

    

}%end of describe
See the pdf vignette for details.
}
\value{
  \code{sobol.fortran} generates random variables in [0,1). 
  It returns a \eqn{n}x\eqn{dim} matrix, when \code{dim>1} 
  otherwise a vector of length \code{n}.
}
\references{

Bratley P., Fox B.L. (1988),
    \emph{Algorithm 659: Implementing Sobol's Quasirandom
    Sequence Generator},
    ACM Transactions on Mathematical Software 14, 88--100.

Joe S., Kuo F.Y. (1998),
    \emph{Remark on Algorithm 659: Implementing Sobol's Quaisrandom
    Seqence Generator}.

Owen A.B. (2020), \emph{On dropping the first Sobol' point},
\url{https://arxiv.org/abs/2008.08051}.


}
\author{
 Christophe Dutang and Diethelm Wuertz
}
\examples{


# (3) Sobol sequences
#

# uniform variate
sobol.fortran(n = 10, dim = 5, scrambling = 3)


}
\keyword{distribution}
