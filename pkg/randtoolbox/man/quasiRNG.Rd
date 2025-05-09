\name{quasiRNG}
\alias{quasiRNG}
\alias{torus}
\alias{sobol}
\alias{runif.sobol}
\alias{halton}
\alias{runif.halton}
\alias{quasi.randtoolbox}
\title{Toolbox for quasi random number generation}
\description{
	the Torus algorithm, the Sobol and Halton sequences.
}
\usage{
torus(n, dim = 1, prime, init = TRUE, mixed = FALSE, usetime = FALSE, 
                  normal = FALSE, mexp = 19937, start = 1)
sobol(n, dim = 1, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                   mixed = FALSE, method = "C", mexp = 19937, start = 1,
                   maxit = 10)
halton(n, dim = 1, init = TRUE, normal = FALSE, usetime = FALSE, 
                    mixed = FALSE, method = "C", mexp = 19937, start = 1)
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
  \item{prime}{a single prime number or a vector of prime numbers to be used
  		 in the Torus sequence. (optional argument).}
  \item{mixed}{a logical to combine the QMC algorithm with the SFMT algorithm, default \code{FALSE}.}
  \item{usetime}{a logical to use the machine time to start the Torus sequence, 
  		   default \code{FALSE}, i.e. when \code{usetime=FALSE} the Torus sequence start from the first term.
  		   \code{usetime} is only used when \code{mixed=FALSE}.}
  \item{method}{a character string either \code{"C"}.
      Note that \code{mixed=TRUE} is only available when \code{method="C"}.}		 
  \item{mexp}{an integer for the Mersenne exponent of SFMT algorithm,
      only used when \code{mixed=TRUE}.}    
  \item{start}{an integer 0 or 1 to initiliaze the sequence, default to 1,
      only used when \code{init=TRUE}.}
  \item{maxit}{a positive integer used to control inner loops both
      for generating randomized seed and for controlling
      outputs (when needed).}    
}
\details{

Scrambling is temporarily disabled and will be reintroduced in a future release.

The currently available generator are given below.
Whatever the sequence, when \code{normal=TRUE}, outputs are transformed with
the quantile of the standard normal distribution \code{\link{qnorm}}.
If \code{init=TRUE}, the default, unscrambled and unmixed-SFMT quasi-random 
sequences start from \code{start}.
If \code{start != 0} and \code{normal=FALSE}, 
we suggest to use 0 as recommended by Owen (2020). 
One must handle the starting value (0) correctly if a quantile
function of a non-lower-bounded distribution is used.

\describe{
    \item{\bold{Torus algorithm:}}{
	The \eqn{k}th term of the Torus algorithm in d dimension is given by
	    	\deqn{u_k = \left(frac(k \sqrt{p_1}), ..., frac(k \sqrt{p_d}) \right)}{%
			    u_k = (frac(k sqrt(p_1)), ..., frac(k sqrt(p_d)) )}
	where \eqn{p_i} denotes the ith prime number, \eqn{frac} the fractional part
	(i.e. \eqn{frac(x) = x-floor(x)}). We use the 100 000 first prime numbers
	from \url{https://t5k.org/}, thus the dimension is limited to 100 000. 
	If the user supplys prime numbers through
	the argument \code{prime}, we do NOT check for primality and we cast numerics
	to integers, (i.e. \code{prime=7.1234} will be cast to \code{prime=7} before
	computing Torus sequence).
	The Torus sequence starts from \eqn{k=1} when initialized with 
	\code{init = TRUE} and so not depending on machine time 
	\code{usetime = FALSE}. This is the default. When \code{init = FALSE}, the sequence
	is not initialized (to 1) and starts from the last term. 
	We can also use the machine time to start the sequence with \code{usetime = TRUE}, 
	which overrides \code{init} or a randomized when \code{mixed = TRUE}.}

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

    \item{\bold{Halton sequences}}{
    	Calculates a matrix of uniform or normal deviated halton low 
    	discrepancy numbers. Let us note that Halton sequence in dimension is the
	Van Der Corput sequence.
	The Halton sequence starts from \eqn{k=1} when initialized with 
	\code{init = TRUE} (default) and no not depending on machine time 
	\code{usetime = FALSE}. 
	When \code{init = FALSE}, the sequence
	is not initialized (to 1) and starts from the last term. We can also use the
	machine time to start the sequence with \code{usetime = TRUE}, which overrides
	\code{init}.
	Two versions of Halton sequences are available the historical version in 
	Fortran (\code{method = "Fortran"}) and the new version in C (\code{method = "C"}).
  If \code{method = "C"}, \code{mixed} argument can be used to randomized
  the Halton sequences.
	}	

}%end of describe
See the pdf vignette for details.
}
\value{
  \code{torus}, \code{halton} and \code{sobol} generates random variables in [0,1). 
  It returns a \eqn{n}x\eqn{dim} matrix, when \code{dim>1} 
  otherwise a vector of length \code{n}.
}
\references{
Bratley P., Fox B.L. (1988),
\emph{Algorithm 659: Implementing Sobol's Quasirandom Sequence Generator},
ACM Transactions on Mathematical Software 14, 88--100.
\doi{10.1145/42288.214372}

Joe S., Kuo F.Y. (2003),
\emph{Remark on Algorithm 659: Implementing Sobol's Quaisrandom Seqence Generator},
ACM Transactions on Mathematical Software 29, 49--57.
\doi{10.1145/641876.641879}

Joe S., Kuo F.Y. (2008),
\emph{Constructing Sobol sequences with better two-dimensional projections},
SIAM J. Sci. Comput. 30, 2635--2654,
\doi{10.1137/070709359}.    

Owen A.B. (2020), \emph{On dropping the first Sobol' point},
\doi{10.1007/978-3-030-98319-2_4}.

Planchet F., Jacquemin J. (2003), 
\emph{L'utilisation de methodes de simulation en assurance}. 
Bulletin Francais d'Actuariat, vol. 6, 11, 3-69. (available online)

}
\seealso{
    \code{\link{pseudoRNG}} for pseudo random number generation, 
    \code{\link{.Random.seed}} for what is done in R about random number generation. 
}
\author{
 Christophe Dutang and Diethelm Wuertz
}
\examples{
# (1) the Torus algorithm
#
torus(100)

# example of setting the seed
setSeed(1)
torus(5)
setSeed(6)
torus(5)
#the same
setSeed(1)
torus(10)

#no use of the machine time
torus(10, use=FALSE)

#Kolmogorov Smirnov test
#KS statistic should be around 0.0019
ks.test(torus(1000), punif) 
	
#KS statistic should be around 0.0003
ks.test(torus(10000), punif) 

#the mixed Torus sequence
torus(10, mixed=TRUE)
\dontrun{
  par(mfrow = c(1,2))
  acf(torus(10^6))
  acf(torus(10^6, mixed=TRUE))
}

#usage of the init argument
torus(5)
torus(5, init=FALSE)

#should be equal to the combination of the two
#previous call
torus(10)

# (2) Halton sequences
#

# uniform variate
halton(n = 10, dim = 5)

# normal variate
halton(n = 10, dim = 5, normal = TRUE)

#usage of the init argument
halton(5)
halton(5, init=FALSE)

#should be equal to the combination of the two
#previous call
halton(10)

# some plots
par(mfrow = c(2, 2), cex = 0.75)
hist(halton(n = 500, dim = 1), main = "Uniform Halton", 
  xlab = "x", col = "steelblue3", border = "white")

hist(halton(n = 500, dim = 1, norm = TRUE), main = "Normal Halton", 
  xlab = "x", col = "steelblue3", border = "white")
   
# (3) Sobol sequences
#

# uniform variate
sobol(n = 10, dim = 5)

# normal variate
sobol(n = 10, dim = 5, normal = TRUE)

# some plots
hist(sobol(500, 1), main = "Uniform Sobol", 
  xlab = "x", col = "steelblue3", border = "white")

hist(sobol(500, 1, normal = TRUE), main = "Normal Sobol", 
  xlab = "x", col = "steelblue3", border = "white")

#usage of the init argument
sobol(5)
sobol(5, init=FALSE)

#should be equal to the combination of the two
#previous call
sobol(10)

# (4) computation times on a 2022 macbook (2017 macbook / 2007 macbook), mean of 1000 runs
#

\dontrun{
# algorithm			time in seconds for n=10^6
# Torus algo					  0.00689 (0.012 / 0.058)
# mixed Torus algo 	    0.009354 (0.018 / 0.087)
# Halton sequence				0.023575 (0.180 / 0.878)
# Sobol sequence				0.010444 (0.027 / 0.214)
n <- 1e+06
mean( replicate( 1000, system.time( torus(n), gcFirst=TRUE)[3]) )
mean( replicate( 1000, system.time( torus(n, mixed=TRUE), gcFirst=TRUE)[3]) )
mean( replicate( 1000, system.time( halton(n), gcFirst=TRUE)[3]) )
mean( replicate( 1000, system.time( sobol(n), gcFirst=TRUE)[3]) )
	}

}
\keyword{distribution}
