\name{isRegular}


\title{Checks if a date/time vector is regular}


\alias{isDaily}
\alias{isMonthly}
\alias{isQuarterly}
\alias{isRegular}
\alias{frequency}


\alias{isDaily.timeDate}
\alias{isMonthly.timeDate}
\alias{isQuarterly.timeDate}
\alias{isRegular.timeDate}
\alias{frequency.timeDate}


\description{

    Checks if a date/time vector is regular. i.e. if it is 
    a daily, a monthly, or a quarterly date/time vector. If 
    the date/time vector is regular the frequency can determined
    calling the function frequency.
  
}


\usage{
\method{isDaily}{timeDate}(x)
\method{isMonthly}{timeDate}(x)
\method{isQuarterly}{timeDate}(x)

\method{isRegular}{timeDate}(x)

\method{frequency}{timeDate}(x, \dots)
}


\arguments{

    \item{x}{  
        an R object of class \code{timeSeries}.
    }
    \item{\dots}{
        arguments to be passed
    }
  
}


\details{

    A date/time vector is defined as daily if the vector has not more 
    than one date/time stamp per day.
    
    A date/time vector is defined as monthly if the vector has not more 
    than one date/time stamp per month.
    
    A date/time vector is defined as quarterly if the vector has not more 
    than one date/time stamp per quarter.
    
    A monthly date/time vector is also a daily vector, a quarterly date/time
    vector is also a monthly vector.
    
    A regular date/time vector is either a monthly or a quarterly vector.
    
    NOT yet implemented is the case of weekly vectors.
    
}


\value{

    The \code{is*} functions return \code{TRUE} or \code{FALSE} depending 
    on whether the date/time vector fulfills the condition or not.
    
    The function frequency returns in general 1, for quarterly date/time
    vectors 4, and for monthly vectors 12.
  
}


\examples{
## None
}


\keyword{chron}
