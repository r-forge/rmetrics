\name{durations}


\title{Durations from a Time Series}


\alias{durations}
\alias{durationSeries}



\description{
    
    Compute durations from an object of class 'timeSeries'.  
       
}


\usage{  
durations(x, trim = FALSE, units = c("secs", "mins", "hours"))   

durationSeries(\dots)
}


\arguments{
  
    \item{trim}{
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{units}{
        [durationSeries] - \cr
        a character value or vector which allows to set the 
        units in which the durations are measured. By default durations
        are measured in seconds. 
        }     
    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{

    returns an object of class \code{timeSeries}.
        
}


\examples{
## Load Microsoft Data - 
   data(MSFT) 
   setRmetricsOptions(myFinCenter = "GMT")
   MSFT = MSFT[1:20, "Open"]
   MSFT

## Compute Durations in hours, Use Continuous Returns - 
   durations(MSFT, units = "hours")
}


\keyword{chron}
