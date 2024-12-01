\name{pasteMat}
\alias{pasteMat}

\concept{concatenate}

\title{Concatenate matrix columns, keeping NAs}

\description{

  Concatenate the columns of a matrix or df. Like paste, but any row
  containing one or more NAs gives in an NA in the corresponding element
  of the result. Argument \code{sep} can be a vector, specifying
  different separators between different columns.

}

\usage{
pasteMat(x, ..., sep = NULL)
}

\arguments{
  \item{x}{
    a matrix or data frame. Can also be a vector if one or more
    \sQuote{...} arguments are used.
  }
  \item{\dots}{
    additional arguments to be combined, together with \code{x}, with
    \code{cbind}.
  }
  
  \item{sep}{

    a character vector of separators between the columns, can be of
    length larger than 1. The default is \code{sep = " "} (as for
    \code{paste}.
  }
}

\details{

  If the \code{...} arguments are used, they are combined by the
  equivalent of \code{x <- cbind(x, ...)}.

  \code{pasteMat} gives a result similar to the one that would be
  obtained from \code{paste} if the columns of \code{x} are passed to
  that individually. The main difference is in the treatment of
  \code{NA}s.

  Any row of \code{x} containing one or more \code{NA}s results in an
  \code{NA} in the corresponding element of the result.

  There can be different separators between the columns. This can be
  obtained by setting \code{sep} to be of length greater than one.
  
}

\value{
  a character vector
}

\author{
  Georgi N. Boshnakov
}

\seealso{
  \code{paste},
  \code{timeDate}
}
\examples{
a <- c("a",  NA, "b", NA, "c")
b <- c("x", "y",  NA, NA, "z")

## turns NAs into the string "NA"
paste(a, b)

## keeps NAs in the result
pasteMat(a, b)
pasteMat(cbind(a, b)) # same

dts <- c("1989-09-28", NA, "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55",   "10:34:02",   NA,   "11:18:23")

paste(dts, tms)
## this throws error (since NAs are converted to the string NA):
##   timeDate(paste(dts, tms), FinCenter = "Europe/Zurich")

## these work
td1 <- timeDate(pasteMat(cbind(dts, tms)), FinCenter = "Europe/Zurich")
td2 <- timeDate(pasteMat(dts, tms), FinCenter = "Europe/Zurich")       
identical(td1, td2)                                                    
td1                                                                       
}
\keyword{character}
