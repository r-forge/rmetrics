\name{timeDate}
\alias{timeDate}

\alias{timeDate,ANY-method}
\alias{timeDate,character-method}
\alias{timeDate,Date-method}
\alias{timeDate,missing-method}
\alias{timeDate,numeric-method}
\alias{timeDate,POSIXt-method}
\alias{timeDate,timeDate-method}
\alias{initialize,timeDate-method}

\alias{as.timeDate}
\alias{as.timeDate.default}
\alias{as.timeDate.POSIXt}
\alias{as.timeDate.Date}
\alias{as.timeDate.timeDate}

\alias{coerce,ANY,timeDate-method}
\alias{coerce,Date,timeDate-method}
\alias{coerce,POSIXt,timeDate-method}


\alias{strptimeDate}

\alias{getDataPart,timeDate-method}
\alias{timeDate,timeDate,ANY-method}



\title{Create 'timeDate' objects}

\description{

  Create a \code{"timeDate"} object from scratch from a character vector
  or other suitable objects.

}

\usage{
timeDate(charvec, format = NULL, zone = "", FinCenter = "", \dots)

\S4method{timeDate}{character}(charvec, format = NULL, zone = "", FinCenter = "", 
         dst_gap = "+")

## methods for as.timeDate
\method{as.timeDate}{default}(x, zone = "", FinCenter = "")  

\method{as.timeDate}{POSIXt}(x, zone = "", FinCenter = "")

\method{as.timeDate}{Date}(x, zone = "", FinCenter = "")

\method{as.timeDate}{timeDate}(x, zone = x@FinCenter, FinCenter = "")  

strptimeDate(x, format = whichFormat(x), tz = "")
}

\arguments{

  \item{charvec}{
    a character vector or vector of dates and times.
  }
  \item{format}{
    the format specification of the input character vector.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{FinCenter}{
    a character with the location of the financial center named as
    "continent/city".
  }
  \item{dst_gap}{
    
    a character string specifying what to do with non-existent times
    falling in a DST gap: add an hour (\code{"+"}), subtract an hour
    (\code{"-"}), set to \code{NA} (\code{"NA"}), or ignore
    (\code{""}). When the \sQuote{ignore} option is used the code to
    check for this kind of faulty times is skipped and the result will
    be equivalent to \code{"+"} or \code{"-"} but which one is not
    defined. This could be useful when you are certain that there are no
    times in DST gaps or don't care how they are dealt with.

  }
  \item{x}{
    
    for \code{strptimeDate}, a character string or vector of dates and
    times.  For the \code{as.timeDate} methods, an object from a class
    that can be converted to \code{"timeDate"}. The default method
    converts \code{x} to \code{character}.

  }
  \item{tz}{
    a character with the location of the financial center 
    named as \code{"continent/city"}, or short \code{"city"}.
  }
  \item{...}{further arguments for methods.}
}

\details{

  \code{timeDate} creates objects from class \code{"timeDate"} from
  character vectors, objects from several date/time classes, and other
  suitable objects.. It is an S4 generic function and this page
  describes the methods defined in package \pkg{timeDate}, see section
  \sQuote{Methods}.

  Note that \code{zone} is the time zone of the input, while
  \code{FinCenter} is the \sQuote{current} time zone, typically but not
  necessarilly where the code is run. To change one or both of these
  time zones of an existing \code{"timeDate"} object, call
  \code{timeDate()} on it, see the method for \code{charvec = "timeDate"}
  in section \sQuote{Methods}.

  The methods for \code{as.timeDate} call \code{timeDate}, maybe after
  some minor preparation. The default method for \code{as.timeDate}
  converts \code{x} to \code{character} before calling \code{timeDate}.
  

  \code{strptimeDate} is a wrapper of \code{timeDate}, suitable when
  \code{zone} and \code{FinCenter} are the same, It has the same
  arguments as \code{\link{strptime}}. If \code{format} is missing it
  tries to deduce it. If \code{tz} is missing it sets it to the value of
  the Rmetrics option \code{"myFinCenter"}.
 
}

\value{
  an object of class \code{"timeDate"}
}

\section{Methods}{

  The following methods for \code{timeDate} are defined in package
  \pkg{timeDate}.

  \describe{

    \item{\code{signature(charvec = "ANY")}}{
      Converts \code{charvec} to \code{character} and calls
      \code{timeDate} on the result.
    }

    \item{\code{signature(charvec = "character")}}{
      ...
    }

    \item{\code{signature(charvec = "Date")}}{
      ...
    }

    \item{\code{signature(charvec = "missing")}}{
      Returns the current time as \code{"timeDate"} object.
    }

    \item{\code{signature(charvec = "numeric")}}{
      ...
    }

    \item{\code{signature(charvec = "POSIXt")}}{
      ...
    }

    \item{\code{signature(charvec = "timeDate")}}{

      Changes the time zone and/or financial center of \code{charvec} to
      the requested ones. If \code{zone} is missing or equal to the
      empty string, just changes the financial center. 
   
    }
  }
}
\seealso{

  \code{\link[=as.character.timeDate]{as.character}},
  \code{\link[=as.POSIXct.timeDate]{as.POSIXct}},
  etc., for conversion from \code{"timeDate"} to other classes
  
}
\examples{
## character vector strings:
dts <- c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")

dts; tms

t1 <- timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "GMT" )
t1

stopifnot(identical(t1, timeDate(dts, FinC = "GMT"))) # auto-format

timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")

timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
     zone = "GMT", FinCenter = "GMT")

timeDate(paste(dts, tms), 
     zone = "Europe/Zurich", FinCenter = "Europe/Zurich")

timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
     zone = "GMT", FinCenter = "Europe/Zurich")

## non standard format:
timeDate(paste(20:31, "03.2005", sep="."), format = "\%d.\%m.\%Y")

## ISO and American formats are auto-detected:
timeDate("2004-12-31", FinCenter = "GMT")
timeDate("12/11/2004", FinCenter = "GMT")
timeDate("1/31/2004") # auto-detect American format

## ... from POSIX?t, and containing NAs:
lsec <- as.POSIXlt(.leap.seconds)
lsec
lsec[c(2,4:6)] <- NA
timeDate(lsec)

dtms <- paste(dts,tms)
dtms[2:3] <- NA
timeDate(dtms, FinCenter = "Europe/Zurich")

## NAs in dates and/or times
dts2 <- c("1989-09-28", NA, "2004-08-30", "1990-02-09")
tms2 <- c(  "23:12:55",   "10:34:02",   NA,   "11:18:23")
## this throws error (since NAs are converted to the string "NA"):
## timeDate(paste(dts,tms), FinCenter = "Europe/Zurich")
## ## Error in midnightStandard2(charvec, format) :
## ##   'charvec' has non-NA entries of different number of characters
##
## these work:
td1 <- timeDate(pasteMat(cbind(dts, tms)), FinCenter = "Europe/Zurich")
td2 <- timeDate(pasteMat(dts, tms), FinCenter = "Europe/Zurich")
identical(td1, td2) ## TRUE

## NA's that appear due to non-existent times;
## on 27/03/1983 in Sofia the clock jumped at midnight to 1am
Sofia_to_DST_char <- c("1983-03-26 23:00:00",
    "1983-03-27 00:00:00", # change to DST; 0am doesn't exist in Sofia on this date
    "1983-03-27 01:00:00",
    "1983-03-27 02:00:00",
    "1983-03-27 03:00:00")

## by default, the non-existent time is moved to the next valid time,
## this is equivalent to dst_gap = "+"
Sofia_to_DST <- timeDate(Sofia_to_DST_char, zone = "Sofia", FinCenter = "Sofia")

## use dst_gap = "NA" to turn invalid times into NA's
Sofia_to_DSTa <- timeDate(Sofia_to_DST_char, zone = "Sofia", FinCenter = "Sofia",
                          dst_gap = "NA")
Sofia_to_DSTa

cbind(Sofia_to_DST_char,
      Sofia_to_DST = format(Sofia_to_DST),
      Sofia_to_DSTa = format(Sofia_to_DSTa)
      )

## dst_gap = "-" rolls the invalid time back
Sofia_to_DSTb <- timeDate(Sofia_to_DST_char, zone = "Sofia", FinCenter = "Sofia",
                          dst_gap = "-")
Sofia_to_DSTb

## Coerce a 'Date' object into a 'timeDate' object:
as.timeDate(Sys.Date())
}

\keyword{chron}
