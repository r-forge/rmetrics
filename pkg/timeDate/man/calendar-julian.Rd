\name{julian}

\alias{julian,timeDate-method}

\alias{atoms}
\alias{atoms,ANY-method}
\alias{atoms,timeDate-method}

\alias{months}
\alias{months,timeDate-method}



\title{Julian counts and calendar atoms}

\description{
    
  Returns Julian day counts, date/time atoms from a \code{"timeDate"} object,
  and extracts month atoms from a \code{"timeDate"} object.
       
}


\usage{
\S4method{julian}{timeDate}(x, origin = timeDate("1970-01-01"), 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"), 
    zone = NULL, FinCenter = NULL, \dots)

\S4method{atoms}{timeDate}(x, \dots)

\S4method{months}{timeDate}(x, abbreviate = NULL)
}


\arguments{
  
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{origin}{
    a length-one object inheriting from class \code{"timeDate"}
    setting the origin for the julian counter.
  }
  \item{units}{
    a character string denoting the date/time units in which the 
    results are desired.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{FinCenter}{
    a character string with the location of the  
    financial center named as "continent/city". 
  }   
  \item{abbreviate}{
    currently not used.
  }   
  \item{\dots}{
    arguments passed to other methods.
  }
}

\details{

  Generic functions to extract properties of \code{"timeDate"}
  objects. \code{julian} and \code{months} are generics from base R,
  while \code{atoms} is a generic defined in this package.

  \code{julian} extracts the number of days since \code{origin} (can be
  fractional), see also \code{\link[base]{julian}}.
  
  \code{atoms} extracts the calendar atoms from a \code{"timeDate"}
  object, i.e., the year, month, day, and optionally, hour, minute and
  second.  The result is a data frame with the financial center in
  atrribute \code{"control"}.

  \code{months} extracts the months as integers from 1 to 12, unlike
  \code{base::months} which returns the names of the months.

}

\value{
  
  for \code{julian}, a \code{difftime} object;
    
  for \code{atoms}, a \code{data.frame} with attribute \code{"control"}
  containing the financial center of the input vector \code{x}.  The
  data frame has the following components:

  \item{Y}{year,}
  \item{m}{month,}
  \item{d}{day,}
  \item{H}{hour,}
  \item{M}{minute,}
  \item{S}{scond;}

  for \code{months}, a numeric vector with attribute \code{"control"}
  containing the financial center.
    
}

\seealso{
  the base R functions
  \code{\link[base]{julian}},
  \code{\link[base]{difftime}},
  \code{\link[base]{months}};

  \code{\link{dayOfWeek}},
  \code{\link{dayOfYear}}
  
}

\examples{
## julian -
   tC = timeCalendar(2022)
   julian(tC)[1:3]
   
## atoms -
   atoms(tC)
   
## months -
   months(tC)
}

\keyword{chron}
