\name{blockStart}
\alias{blockStart}

\alias{blockEnd}


\title{Equally sized 'timeDate' blocks}

\description{
  
  Creates start (end) dates for equally sized \code{"timeDate"} blocks.
  
}

\usage{
blockStart(x, block = 20)
blockEnd(x, block = 20)
}

\arguments{
  
  \item{block}{
    an integer value specifying the length in number of records
    for numerically sized blocks of dates.
  }
  \item{x}{
    an object of class \code{"timeDate"}.
  }
}

\details{

  The functions \code{blockStart} and \code{blockEnd} create vectors of
  start and end values for equally sized \code{"timeDate"} blocks. Note,
  the functions are event counters and not a time counter between
  measuring time intervals between start and end dates! For equally
  sized blocks in time one has before to align the time stamps in equal
  time differences.
  
}

\value{
  an object of class \code{"timeDate"}
}

\examples{  
## timeSequence
# 360 Days Series:
tS <- timeSequence(to = "2022-09-23 09:39:23", length.out = 360)
    
## blockStart | blockEnd
Start <- blockStart(tS, 30)
End <- blockEnd(tS, 30)
Start
End
End - Start
}

\keyword{chron}
