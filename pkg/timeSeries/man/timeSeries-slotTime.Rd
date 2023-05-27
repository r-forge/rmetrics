\name{time}


\title{Get and set time stamps of a 'timeSeries'}


\alias{time}
\alias{time<-}

\alias{time,timeSeries-method}
\alias{time.timeSeries}

\alias{time<-.timeSeries}

\alias{getTime}  
\alias{setTime<-}


\description{
    
    Functions and methods extracting and modifying positions 
    of 'timeSeries' objects.
        
}


\usage{
\S4method{time}{timeSeries}(x, \dots)
\method{time}{timeSeries}(x) <- value

getTime(x)         
setTime(x) <- value
}


\arguments{

  \item{value}{
    a valid value for the time component of \code{x}.
  }
  \item{x}{
    an object of class \code{timeSeries}.
  } 
  \item{\dots}{
    optional arguments passed to other methods.
  }
}

\details{
  
  \code{time} and \code{time<-} are generic functions with methods for
  class \code{"timeSeries"}. They get and set the time component of the
  object. 

  \code{getTime} and \code{setTime} are non-generic alternatives are
  non-generic wrappers of \code{time} and \code{time<-}, respectively.

}

\value{

  for \code{time} and \code{getTime}, a 'timeDate' object,

  for \code{time<-} and and \code{setTime}, the modified 'timeSeries' object.
           
}

\note{

  There is a generic function \code{time<-} defined in package
  \pkg{zoo}, as well. When that package is loaded its \code{time<-} also
  gets the \code{"timeSeries"} method. 

  This works transparantly, if \pkg{zoo} is attached after
  \pkg{timeSeries} or if \pkg{timeSeries} is loaded but not attached.
  However, if \pkg{timeSeries} is attached after \pkg{zoo},
  \code{zoo::`time<-`} will be masked, so \code{time(x) <- value} will
  see only the methods for the \pkg{timeSeries} version of \code{time<-}.

  There is no such problem for \code{time} since it is defined in base \R.

}

\examples{   
## Create Dummy 'timeSeries' -      
   X <- timeSeries(matrix(rnorm(24), 12), timeCalendar())
   
## Return Series Positions -
   getTime(X)
   time(X)  
  
## Add / Subtract one Day from X
   setTime(X) <- time(X) - 24*3600 # sec
   X
   time(X) <- time(X) + 24*3600 # sec
   X
}


\keyword{chron}

