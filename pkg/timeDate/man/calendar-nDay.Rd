\name{nDay}
\alias{nDay}

\alias{timeNthNdayInMonth}
\alias{timeLastNdayInMonth}


\title{n-th n-day dates}

\description{

  Computes the date for the n-th or last occurrence of an n-day in
  year/month.

}
\usage{
timeNthNdayInMonth(charvec, nday = 1, nth = 1, format = "\%Y-\%m-\%d",
    zone = "", FinCenter = "")

timeLastNdayInMonth(charvec, nday = 1, format = "\%Y-\%m-\%d",
    zone = "", FinCenter = "")
}

\arguments{
  \item{charvec}{a character vector of dates and times.}
  \item{nday}{an integer vector with entries ranging from \code{0}
    (Sunday) to \code{6} (Saturday).}
  \item{nth}{an integer vector numbering the n-th occurence.}
  \item{format}{the format specification of the input character vector.}
  \item{zone}{the time zone or financial center where the data were recorded.}
  \item{FinCenter}{a character with the location of the financial
    center named as "continent/city".}
}

\details{

  \code{timeNthNdayInMonth} returns the \code{nth} occurrence of a n-day
  (nth = 1,...,5) in \code{year}, \code{month}.

  \code{timeLastNdayInMonth} returns the last \code{nday} in
  \code{year}, \code{month}.

}

\value{
  an object of class \code{"timeDate"}
}

\examples{
## timeNthNdayInMonth
# What date is the second Monday in April 2004?
timeNthNdayInMonth("2004-04-01", 1, 2)

## timeLastNdayInMonth
# What date has the last Tuesday in May, 1996?
timeLastNdayInMonth("1996-05-01", 2)
}

\keyword{chron}
