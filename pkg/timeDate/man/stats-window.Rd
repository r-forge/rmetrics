\name{window}


\title{Time Windows}


\alias{window}
\alias{window.timeDate}
\alias{cut.timeDate}


\description{
    
    Extract the subset of a 'timeDate' object observed between 
    two time stamps.
       
}


\usage{
\method{window}{timeDate}(x, start , end, \dots)

\method{cut}{timeDate}(x, from , to, \dots)
}


\arguments{
  
    \item{from, to}{
        starting date, required, and end date, optional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{start, end}{
        starting date, required, and end date, optional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }         
}


\value{
   
  an object of class \code{timeDate}.

}

\note{

  % :TODO:
  
  The method for \code{cut} has been discouraged in the sources for a
  long time (with a recommendation to use \code{window}). It will be
  officially deprecated in the next release and later removed or
  replaced by a method that is consistent with the methods for
  \code{cut} in base R,

}


\examples{  
## timeCalendar - 
   # Monthly Dates in Current Year:
   tS = timeCalendar()
   tS 
   
## window -
   # 2nd Quarter Window:
   tS[4:6]
   window(tS, tS[4], tS[6])
}


\keyword{chron}

