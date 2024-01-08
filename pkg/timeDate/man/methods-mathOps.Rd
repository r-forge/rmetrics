\name{timeDateMathOps}
\alias{timeDateMathOps}

\alias{Ops,timeDate,timeDate-method}
\alias{+,timeDate,timeDate-method}
\alias{+,timeDate,numeric-method}
\alias{-,timeDate,timeDate-method}
\alias{-,timeDate,numeric-method}
\alias{-,numeric,timeDate-method}
\alias{+,numeric,timeDate-method}


\title{Mathematical operations with 'timeDate' objects}

\description{
    
  Functions for mathematical and logical operations on \code{"timeDate"}
  objects.

}

\usage{
\S4method{Ops}{timeDate,timeDate}(e1, e2)
}

\arguments{
 
  \item{e1, e2}{
    
    objects of class \code{"timeDate"}. In the case of addition and
    subtraction one of them may be of class \code{numeric}, specifying
    the number of seconds to add or subtract.

  }    
}

\details{

  Group \code{"Ops"} represents the binary mathematical operators.
  Methods are defined for such operations when one or both arguments are
  from class \code{"timeDate"}.

  Operations that don't make sense, such as addition of two
  \code{"timeDate"} objects, throw error.
  
  The plus operator \code{"+"} performs arithmetic "+" operation on 
  \code{"timeDate"} objects,

  and the minus operator \code{"-"} returns a 
  \code{difftime} object if both arguments \code{e1} and \code{e2} 
  are \code{"timeDate"} objects, or returns a \code{"timeDate"} object 
  \code{e2} seconds earlier than \code{e1}.
  
}

\value{
  
  addition of numeric to \code{"timeDate"} returns \code{"timeDate"},

  subtraction of numeric from \code{"timeDate"} returns \code{"timeDate"},

  subtraction of two \code{"timeDate"} objects returns \code{"difftime"},

  other operations between two \code{"timeDate"} objects are applied to
  the underlying times (slot \code{"Date"}). The result of that
  operation is converted to \code{"timeDate"} if it represents a time
  and returned as is otherwise.
  
}


\examples{
## create character vectors
dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
dts
tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
tms
   
## "+/-"
# add one day to a given timeDate object
GMT = timeDate(dts, zone = "GMT", FinCenter = "GMT")
GMT
ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")
ZUR
GMT + 24*3600
ZUR[2] - ZUR[1] 
}

\keyword{chron}
