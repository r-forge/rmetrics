\name{runifInterface}
\alias{runifInterface}
\alias{set.generator}
\alias{put.description}
\alias{get.description}
\title{Functions for using runif() and rnorm() with randtoolbox generators}
\description{
These functions allow to set some of the random number generators from randtoolbox
package to be used instead of the standard generator in the functions, which use
random numbers, for example \code{runif()}, \code{rnorm()}, \code{sample()}
and \code{set.seed()}.
}
\usage{
set.generator(name=c("WELL", "MersenneTwister", "default"),
              parameters=NULL, seed=NULL, \dots, only.dsc=FALSE)

put.description(description)		

get.description()
}
\arguments{
	\item{name}{A character string for the RNG name.}
	\item{parameters}{A numeric or character vector describing a specific RNG from the
           family specified by the \code{name}.}
	\item{seed}{A number, whose value is an integer between \code{0} and \code{2^32 - 1},
           to be used as a seed.}
	\item{\dots}{Arguments describing named components of the vector of parameters,
           if argument \code{parameters} is missing or \code{NULL}.}	
	\item{only.dsc}{Logical. If \code{TRUE}, a description of the specified RNG is returned
           and the generator is not initialized.}
	\item{description}{A list describing a specific RNG as created by
           \code{set.generator(,only.dsc=TRUE)} or \code{get.description()}.}
}
\details{
Random number generators provided by R extension packages are set using
\code{RNGkind("user-supplied")}. The package \pkg{randtoolbox} assumes that
this function is not called by the user directly and is called from
the functions \code{set.generator()} and \code{put.description()}.

Random number generators in \pkg{randtoolbox} are represented at the R level by a list
containing mandatory components \code{name, parameters, state} and possibly an
optional component \code{authors}. The function \code{set.generator()} internally
creates this list from the user supplied information and then runs \code{put.description()}
on this list, which initializes the generator. If the generator is initialized, then the
function \code{get.description()} may be used to get the actual state of the generator,
which may be stored in an R object and used later in \code{put.description()} to continue
the sequence of the random numbers from the point, where \code{get.description()}
was called. This may be used to generate several independent streams of random numbers
generated by different generators.

The component \code{parameters} is a character or a numeric vector, whose structure
is different for different types of the generators. This vector may be passed
to \code{set.generator()}, if it is prepared before its call, however, it is
also possible to pass its named components via the \code{...} parametr of
\code{set.generator()} and the vector \code{parameters} is created internally.
If the vector \code{parameters} is not supplied and the arguments in \code{...}
are not sufficient to create it, an error message is produced.

\subsection{Linear congruential generators}{
Currently disabled.

Parameters for the linear congruential generators (\code{name="congruRand"})
are integers represented either as a character or a numeric vector. The
components are
\describe{
  \item{mod}{The modulus.}
  \item{mult}{The multiplier.}
  \item{incr}{The increment.}
}
}

\subsection{WELL generators}{
Parameters for the WELL generators is a character vector with components
\describe{
  \item{order}{The number of bits in the internal state.
   Possible values are 512, 521, 607, 800, 1024, 19937, 21701, 23209, 44497.}
  \item{version}{The version letter "a", "b", or "c" to be appended to the order.}
}
The concatenation of \code{order} and \code{version} should belong to
\code{"512a", "521a", "521b", "607a", "607b", "800a", "800b", "1024a", "1024b",
"19937a", "19937b", "19937c", "21701a", "23209a", "23209b", "44497a", "44497b"}.
When order and version are specified in \code{...} parametr of \code{set.generator()},
then the parameter \code{order} is optional and if missing, it is assumed that the
parameter \code{version} contains also the number of bits in the internal state
and the combination belongs to the list above.
}

\subsection{Mersenne Twister 2002 generator}{
Parameters for the Mersenne Twister 2002 is a character vector with components
\describe{
  \item{initialization}{Either "init2002" or "array2002". The initialization to be used.}
  \item{resolution}{Either 53 or 32. The number of random bits in each number.}
}
}

}
\value{
\code{set.generator()} with the parameter \code{only.dsc=TRUE} and
\code{get.description()} return the list describing a generator.
\code{put.description()} with the parameter \code{only.dsc=TRUE} (the default)
and \code{put.description()} return \code{NULL}.
}
\seealso{
\code{\link{RNGkind}} and \code{\link{.Random.seed}} for random number generation in R.
}
\author{
 Petr Savicky and Christophe Dutang
}
\examples{
#set WELL19937a
set.generator("WELL", version="19937a", seed=12345)
runif(5)

#Store the current state  and generate 10 random numbers
storedState <- get.description()
x <- runif(10)

#Park Miller congruential generator
set.generator(name="congruRand", mod=2^31-1, mult=16807, incr=0, seed=12345)
runif(5)
setSeed(12345)
congruRand(5, dim=1, mod=2^31-1, mult=16807, incr=0)

# the Knuth Lewis RNG
set.generator(name="congruRand", mod="4294967296", mult="1664525", incr="1013904223", seed=1)
runif(5)
setSeed(1)
congruRand(5, dim=1, mod=4294967296, mult=1664525, incr=1013904223)

#Restore the generator from storedState and regenerate the same numbers
put.description(storedState)
x == runif(10)

# generate the same random numbers as in Matlab
set.generator("MersenneTwister", initialization="init2002", resolution=53, seed=12345)
runif(5)
# [1] 0.9296161 0.3163756 0.1839188 0.2045603 0.5677250
# Matlab commands rand('twister', 12345); rand(1, 5) generate the same numbers,
# which in short format are   0.9296    0.3164    0.1839    0.2046    0.5677

#Restore the original R setting
set.generator("default")
RNGkind()

}
\keyword{distribution}

