\name{listHolidays}
\alias{listHolidays}


\title{List of holidays}

\description{
    
  Returns a list of holidays.     
                
}


\usage{
listHolidays(pattern = ".*") 
}


\arguments{
  
  \item{pattern}{
    a pattern character string as required by the \code{\link{grep}}
    function.
  }

}

\details{

  Returns a character vector containing the names of supported holidays
  matching \code{pattern}. The default is to return all holidays.

}

\value{
  a character vector
}


\examples{
## listHolidays -

   # All Holidays:
   listHolidays()
   
   # Local Swiss Holidays:
   listHolidays("CH") 
}

\keyword{chron}
