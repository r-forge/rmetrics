\name{aggregate-methods}

\docType{methods}
\alias{aggregate}
\alias{aggregate.timeSeries}
\alias{aggregate-methods}
\alias{aggregate,timeSeries-method}


\title{timeSeries Class, Functions and Methods}

\description{
    
    Aggregates a 'timeSeries' Object.      
    
}


\usage{
\S4method{aggregate}{timeSeries}(x, by, FUN, \dots)

}


\arguments{
  
    \item{by}{
        [aggregate] - \cr
        sequence of \code{timeDate} objects  denoting the aggregation
        period.
        }
    \item{FUN}{
        the function to be applied.
        }               
%    \item{units}{
%        an optional character string, which allows to overwrite the 
%        current column names of a \code{timeSeries} object. By default 
%        \code{NULL} which means that the column names are selected
%        automatically.
%        }
    \item{x}{
        an object of class \code{timeSeries}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    returns an aggregated S4 object of class \code{timeSeries}.
        
}


\examples{
## Load Microsoft Data Set -
   data(MSFT)
   x <- MSFT

## Aggregate by Weeks - 
   by <- timeSequence(from = start(x),  to = end(x), by = "week")
   aggregate(x, by, mean)

## Aggregate to Last Friday of Month -
   by <- unique(timeLastNdayInMonth(time(x), 5))
   aggregate(x, by, mean)

## Aggregate to Last Day of Quarter -
   by <- unique(timeLastDayInQuarter(time(x)))
   aggregate(x, by, mean)
}


\keyword{methods}
\keyword{chron}
