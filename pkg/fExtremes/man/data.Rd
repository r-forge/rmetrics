\name{TimeSeriesData}

\alias{TimeSeriesData}

\alias{bmwRet}
\alias{danishClaims}


\title{Time Series Data Sets} 

\usage{
bmwRet
danishClaims
}

\description{
Data sets used in the examples of the fExtremes packages.
}

\format{
\code{bmwRet}. A data frame with 6146 observations on 2 variables. The first column contains dates (Tuesday 2nd January 1973 until Tuesday 23rd July 1996) and the second column contains the respective value of daily log returns on the BMW share price made on each of those dates.  These data are an irregular time series because there is no trading at weekends.

\code{danishClaims}. A data frame with 2167 observations on 2 variables.  The first column contains dates and the second column contains the respective value of a fire insurance claim in Denmark made on each of those dates. These data are an irregular time series.
}

\examples{
head(bmwRet)
head(danishClaims)
}
