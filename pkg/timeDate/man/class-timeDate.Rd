\name{timeDate-class}
\alias{timeDate-class}

\docType{class}


\title{Class "timeDate"}

\description{ 
  Class \code{"timeDate"} represents date and time objects.
}

\section{Objects from the Class}{

  Objects can be created by calls of the functions
  \code{\link{timeDate}}, \code{\link{timeSequence}},
  \code{\link{timeCalendar}} and \code{\link{as.timeDate}}, among
  others. There is also a \code{"timeDate"} method for
  \code{\link[=seq.timeDate]{seq}}.
  
}

\section{Slots}{
  \describe{

    \item{\code{Data}:}{Object of class \code{"POSIXct"}: 
      a vector of POSIXct dates and times always related to
      "GMT".
    } 
    \item{\code{format}:}{Object of class \code{"character"}: 
      a character string denoting the format specification of 
      the input data character vector.
    }
    \item{\code{FinCenter}:}{Object of class \code{"character"}: 
      a character string with the location of the
      financial center named as "continent/city", or just "city".
    }
    
  }
}   

\details{

  For the management of chronological objects under R three concepts
  are available: The first is the implementation of date and time in R's
  \code{chron} package neglecting locals, time zones and day light saving
  times. This approach is in most cases appropriate for economic time 
  series. The second approach, available in R's base package implements 
  the POSIX standard to date and time objects, named \code{"POSIXt"}. 
  
  Unfortunately, the representation of these objects is in some cases
  operating system dependent and especially under MS Windows several 
  problems appeared over the time in the management of time zones and 
  day light saving times. Rmetrics overcomes these difficulties with 
  POSIX objects and introduce a new S4 class of \code{"timeDate"} objects 
  which allow for powerful methods to represent dates and times in 
  different financial centers around the world. 
  
  Many of the basic functionalities of these objects are in common 
  with S-Plus' \code{"timeDate"} objects and thus many of your privately 
  written functions for SPlus/FinMetrics may also be used within the
  R/Rmetrics environment.

  A major difference is the time zone concept which is replaced by the
  "Financial Center" concept. The \code{FinCenter} character variable
  specifies where you are living and at which financial center you are
  working. With the variable \code{myFinCenter} you can overwrite the
  default setting with your personal settings. With the specification 
  of the \code{FinCenter} your system knows what rules rules for day 
  light saving times should be applied, what is your holiday calendar, 
  what is your currency, what are your interest rate conventions. 
  (Not all specifications are already implemented.) Many other
  aspects can be easily accessed when a financial center is named. So we
  can distinguish between Frankfurt and Zurich, which both belong to the
  same time zone, but differed in DST changes in the eighties and have
  different holiday calendars. Futhermore, since the underlying time
  refers to "GMT" and DST rules and all other information is available
  in local (ASCII) databases, we are sure, that R/Rmetrics delivers with 
  such a date/time concept on every computer independent of the operating 
  systemin use, identical results.

  Another important feature of the \code{"timeDate"} concept used here is
  the fact that we don't rely on American or European ways to write dates.
  We use consequently the ISO-8601 standard for date and time notations. \cr
  
  \bold{Generation of \code{"timeDate"} Objects}

  We have defined a \code{"timeDate"} class which is in many aspects similar
  to the S-Plus class with the same name, but has also some important
  advantageous differeneces. The S4 class has four Slots, the \code{Data}
  slot which holds date and time as 'POSIXct' objects in the standard
  ISO-8601 format, the \code{Dim} slot which gives the dimension
  of the data object (i.e. its length), the \code{format} specification 
  slot and the \code{FinCenter} slot which holds the name of the financial 
  center. By default this is the value 

  Three functions allow to cgenerate date/time objects: \code{"timeDate"}
  from character vectors, \code{timeCalendar} from date and time atoms,
  and \code{timeSequence} from a "from/to" or from a "from/length" sequence
  specification. Note, time zone transformations are easily handled by
  by the \code{"timeDate"} functions which can also take \code{"timeDate"}
  and \code{POSIXt} objects as inputs, while transforming them between
  financial centers and/or time zones specified by the arguments \code{zone}
  and \code{FinCenter}. Finally the function \code{Sys.timeDate} returns
  current system time in form of a \code{"timeDate"} object.\cr
  
  
  \bold{Tests and Representation of timeDate Objects:}

  Rmetrics has implemented several methods to represent
  \code{"timeDate"} objects. For example, the 
  print method returns the date/time in square \code{"[]"} brackets 
  to distinguish the output from other date and time objects. 
  On top of the date and
  time output the name of the \code{FinCenter} is printed. The summary
  method returns a printed report with information about the
  \code{"timeDate"} object. Finally, the format methods allows to
  transform objects into a ISO conform formatted character strings. \cr


  \bold{Mathematical Operations:}

  Rmetrics supports methods
  to perform many mathematical operations. Included are methods 
  to extract or 
  to replace subsets from \code{"timeDate"} objects, 
  to perform arithmetic \code{"+"} and \code{"-"} operations, 
  to group \code{\link{Ops}} generic functions,
  to return suitably lagged and iterated differences \code{\link{diff}}, 
  to return differences \code{\link{difftimeDate}} of two \code{"timeDate"} 
  objects, to concatenate objects, to replicate objects, 
  to \code{\link{round}} objects, 
  to truncate objects using \code{\link{trunc}}, 
  to extract the first or last entry of a vector, 
  to \code{\link{sort}} the objects of the elements of a date/time vector, and 
  to revert \code{"timeDate"} vector objects, among other functions. \cr


  \bold{Transformation of Objects:}

  Rmetrics has also functions to transform dat/time objects between 
  different representations. Included are methods to transform 
  \code{"timeDate"} objects to character strings, to data frames, 
  to POSIXct or POSIXlt objects, to \code{\link{julian}}
  counts. One can extract date/time \code{atoms} from calendar 
  dates, and the \code{\link{months}} atoms from a \code{"timeDate"} 
  object.
  
}

\note{

  Originally, these functions were written for Rmetrics users using R and
  Rmetrics under Microsoft's Windows XP operating system where time zones,
  daylight saving times and holiday calendars are not or insufficiently
  supported. 

  The usage of the Ical Library and the introduction of the FinCenter
  concept was originally develloped for R Version 1.5. The
  \code{"timeDate"} and \code{timeSeries} objects were added for
  R Version 1.8.1. Minor changes were made to adapt the functions
  for R Version 1.9.1. As a consequence, newer concepts like the
  \code{Date} objects were not yet considered and included in this
  collection of date and time concepts. With R Version 2.3.0 a major
  update has been made adding many new generic functions and renaming
  a few already existing functions, please be aware of this.

  Note, the date/time conversion from an arbitrary time zone to GMT cannot
  be unique, since date/time objects appear twice during the hour
  when DST changes and the isdt flag was not recorded. A bookkeeping 
  which takes care if DST is effective or not is not yet included. 
  However, in most applications this is not necessary since the markets 
  are closed on weekends, especially at times when DST usually changes. 
  It is planned for the future to implement the DST supporting this
  facility.

  The ISO-8601 midnight standard has been implemented. Note, that for
  example "2005-01-01 24:00:00" is accepted as a valid date/time string.
  
  Also available is an automated format recognition, so the user does
  not have to specify the format string for the most common date/time
  formats.

}

\section{Methods}{
  
  \describe{
    \item{timeDate}{\code{signature(charvec = "timeDate")}:
      create objects from class \code{"timeDate"}, see \code{\link{timeDate}};
    }
      
    \item{show}{\code{signature(object = "timeDate")}: 
      prints an object of class \code{"timeDate"};
    }  
  
    \item{plot}{\code{signature(x = "timeDate")}:  }
    \item{points}{\code{signature(x = "timeDate")}:  }
    \item{lines}{\code{signature(x = "timeDate")}:  }
    \item{abline}{\code{signature(a = "ANY", b = "ANY", h = "ANY", v = "timeDate")}:
      see \code{\link{plot-methods}}.
    }

    \item{$}{\code{signature(x = "timeDate")}: ... }

    \item{[}{\code{signature(x = "timeDate", i = "ANY", j = "missing")}:  }
    \item{[}{\code{signature(x = "timeDate", i = "character", j = "missing")}:  }
    \item{[}{\code{signature(x = "timeDate", i = "logical", j = "missing")}:  }
    \item{[}{\code{signature(x = "timeDate", i = "missing", j = "missing")}:  }
    \item{[}{\code{signature(x = "timeDate", i = "numeric", j =	"missing")}:  
      take parts of a \code{"timeDate"} object, see \code{\link{subset}}.
    }

    \item{finCenter}{\code{signature(x = "timeDate")}: }
    \item{finCenter<-}{\code{signature(x = "timeDate")}:
      see \code{\link{finCenter}}.
    }
    
    \item{atoms}{\code{signature(x = "timeDate")}: }

    \item{align}{\code{signature(x = "timeDate")}:
      see \code{\link{align}}.
    }

    \item{isDaily}{\code{signature(x = "timeDate")}:  }
    \item{isMonthly}{\code{signature(x = "timeDate")}:  }
    \item{isQuarterly}{\code{signature(x = "timeDate")}:  }
    \item{isRegular}{\code{signature(x = "timeDate")}:
      see see \code{\link{isRegular}}.
    }
    \item{frequency}{\code{signature(x = "timeDate")}:
      see \code{\link{frequency}}.
    }

    \item{is.na}{\code{signature(x = "timeDate")}:
      see \code{\link{is.na-methods}}.
    }
    
    \item{sample}{\code{signature(x = "timeDate")}:
      see \code{\link{sample}}.
    }

    \item{Ops}{\code{signature(e1 = "timeDate", e2 = "timeDate")}:  }
    \item{+}{\code{signature(e1 = "numeric", e2 = "timeDate")}:  }
    \item{+}{\code{signature(e1 = "timeDate", e2 = "numeric")}:  }
    \item{+}{\code{signature(e1 = "timeDate", e2 = "timeDate")}:  }
    \item{-}{\code{signature(e1 = "numeric", e2 = "timeDate")}:  }
    \item{-}{\code{signature(e1 = "timeDate", e2 = "numeric")}:  }
    \item{-}{\code{signature(e1 = "timeDate", e2 = "timeDate")}:
      see \code{\link{timeDateMathOps}}.
    }
    
    \item{coerce}{\code{signature(from = "ANY", to = "timeDate")}:  }
    \item{coerce}{\code{signature(from = "Date", to = "timeDate")}:  }
    \item{coerce}{\code{signature(from = "POSIXt", to = "timeDate")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "character")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "data.frame")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "Date")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "list")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "numeric")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "POSIXct")}:  }
    \item{coerce}{\code{signature(from = "timeDate", to = "POSIXlt")}:
      convert from/to \code{"timeDate"} objects. 
      These are methods for \code{as}, to be used with the syntax
      \code{as(from, to)}, where \code{from} is the object to be
      converted and \code{to} is the desired target class. Most
      conversions can also be done with specialised functions such as
      \code{as.character} and \code{as.timeDate}, see
      \code{\link{as.timeDate}}.
    }


    \item{names}{\code{signature(x = "timeDate")}: }
    \item{names<-}{\code{signature(x = "timeDate")}:
      see \code{\link{names-methods}}.
    }
    
    \item{getDataPart}{\code{signature(object = "timeDate")}: ... }
    \item{initialize}{\code{signature(.Object = "timeDate")}: ... }

  }
}  

\examples{
## Examples for Objects of class 'timeDate'

## Sys.timeDate()           # direct
## timeDate(Sys.time())     # transformed from "POSIX(c)t"

## local time in Zurich
## timeDate(Sys.time(), FinCenter = "Zurich")  

## character vector strings for the examples below
dts <- c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")

t1 <- timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "GMT" )
t1a <- timeDate(dts, FinCenter = "GMT")  # auto-format
identical(t1, t1a)

t1

timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")

timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
  zone = "GMT", FinCenter = "GMT")

timeDate(paste(dts, tms), 
  zone = "Europe/Zurich", FinCenter = "Europe/Zurich")

timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
  zone = "GMT", FinCenter = "Europe/Zurich")

## non standard format
timeDate(paste(20:31, "03.2005", sep="."), format = "\%d.\%m.\%Y")

## Note, ISO and American formats are auto-detected
timeDate("2004-12-31", FinCenter = "GMT")
timeDate("12/11/2004", FinCenter = "GMT")
timeDate("1/31/2004") # auto-detect American format

## ... from POSIX?t, and Using NAs:
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
## this throws error (since NAs are converted to the string NA):
timeDate(paste(dts,tms), FinCenter = "Europe/Zurich")
## Error in midnightStandard2(charvec, format) :
##   'charvec' has non-NA entries of different number of characters

## these work:
td1 <- timeDate(pasteMat(cbind(dts, tms)), FinCenter = "Europe/Zurich")
td2 <- timeDate(pasteMat(dts, tms), FinCenter = "Europe/Zurich")
identical(td1, td2) ## TRUE

## timeCalendar
## getRmetricsOptions("currentYear")
timeCalendar()  # 12 months of current year
timeCalendar(2022) # 12 months of 2022
timeCalendar(y = c(1989, 2001, 2004, 1990), 
    m = c(9, 1, 8, 2), d = c(28, 15, 30, 9), FinCenter = "GMT")
timeCalendar(y = c(1989, 2001, 2004, 1990),
    m = c(9, 1, 8, 2), d = c(28, 15, 30, 9), FinCenter = "Europe/Zurich")

## timeCalendar(h = c(9, 14), min = c(15, 23))
timeCalendar(2022, h = c(9, 14), min = c(15, 23))

## timeSequence
timeSequence(from = "2004-03-12", to = "2004-04-11",
  format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeSequence(from = "2004-03-12", to = "2004-04-11",
  format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")

## print, summary, format
tC = timeCalendar(2022)
tC
print(tC)
summary(tC)
format(tC)
}

\keyword{chron}
