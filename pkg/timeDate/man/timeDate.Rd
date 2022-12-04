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

\alias{strptimeDate}

\alias{getDataPart,timeDate-method}
\alias{timeDate,timeDate,ANY-method}



\title{Create 'timeDate' objects from scratch}

\description{

    Create a \code{"timeDate"} object from scratch from a character
    vector or other suitable objects.

}


\usage{
timeDate(charvec, format = NULL, zone = "", FinCenter = "", \dots)

\S4method{timeDate}{character}(charvec, format = NULL, zone = "", FinCenter = "", 
         dst_gap = "+")

strptimeDate(x, format = whichFormat(x), tz = "")
}


\arguments{

  \item{charvec}{
    a character string or vector of dates and times.
  }
  \item{format}{
    the format specification of the input character vector.
  }
  \item{tz}{
    a character with the location of the financial center 
    named as \code{"continent/city"}, or short \code{"city"}.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{x}{
    a character string or vector of dates and times.
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



\examples{
## timeDate -

   # Character Vector Strings:
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")

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

   ## Non Standard Format:
   timeDate(paste(20:31, "03.2005", sep="."), format = "\%d.\%m.\%Y")

   ## Note, ISO and American Formats are Auto-Detected:
   timeDate("2004-12-31", FinCenter = "GMT")
   timeDate("12/11/2004", FinCenter = "GMT")
   timeDate("1/31/2004") # auto-detect American format

   ## From POSIX?t, and using NAs
   ## lsec <- as.POSIXlt(.leap.seconds) ; lsec[c(2,4:6)] <- NA
   ## timeDate(lsec)

   ## dtms <- paste(dts,tms)
   ## dtms[2:3] <- NA
   ## timeDate(dtms, FinCenter = "Europe/Zurich") # but in GMT

   % ## would need  change in R :
   % ##   tms[3] <- dts[2] <- NA
   % ##   timeDate(paste(dts,tms), FinCenter = "Europe/Zurich") # but in GMT

}

\keyword{chron}
