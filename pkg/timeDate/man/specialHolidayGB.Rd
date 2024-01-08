\name{specialHolidayGB}
\alias{specialHolidayGB}

\title{
  Dates of special one-off holidays in the UK
}

\description{
    Gives dates of special one-off holidays in the UK.
}

\usage{
specialHolidayGB(year = getRmetricsOptions("currentYear"),
                     value = "timeDate", named = FALSE, ...)
}

\arguments{
  \item{year}{

    the year(s) for which special holidays are required, a vector
    containing four-digit integer number(s) of the form \code{CCYY},
    e.g. \code{2023}.

  }
  \item{value}{
    
    the class of the returned value. If \code{"timeDate"}, the default,
    return a \code{"timeDate"} object, if \code{""} return a character
    vector.
    
  }
  \item{named}{

    if \code{TRUE}, the dates are named, otherwise unnamed.

  }
  \item{\dots}{

    further arguments for \code{as.character} when \code{value = ""}.

  }
}

\details{

  \code{specialHolidayGB} gives the special Bank holidays in England for
  the years specified by argument \code{year}, such as the Millenium day
  at the end of 1999 and significant Royal events. Don't assume that
  there is at most one special holiday in a given year, 2022 had two.

  Years that do not contain special Bank holidays are omitted. If there
  are no special holidays in the specified year(s) the results is a
  \code{"timeDate"} or \code{"character"} object of length zero.

  The holidays are sorted in increasing time order. 
  
  Argument \code{value} controls the class of the result. The default is
  \code{"timeDate"}. The result is a character vector if \code{value =
  ""} (the empty string). In the latter case, further arguments for the
  transformation to \code{character} can be passed in argument
  \code{"..."} (e.g., \code{format}). 
  
  If argument \code{named} is \code{TRUE}, the dates get names
  asssociated with them, so one can see which date represents which
  holiday.
  
}

\value{
  a \code{"timeDate"} or a character vector, as requested by argument
  \code{value}
}

%\references{
%%% ~put references to the literature/web site here ~
%}

\author{
  Georgi N. Boshnakov
}

\note{

  While most of the holidays given by the functions with prefix
  \code{GBxxx} are valid for the UK as a whole and they are (or should be)
  fully correct for England, there are variations in Scotland, Wales and
  Northern Ireland. 

  Functions containing 'London' in their name refer to the London Stock
  Exchange. Currently, the Bank holidays given by those functions are
  the same as for England. Actually, the 'official' holidays between
  1834 and 1870 were set by the Bank of England. The first Act of
  Parlament on the issue is from 1871.
  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  
  \code{\link{GBSummerBankHoliday}} for functions giving specific
  regular Bank holidays,

  \code{\link{holidayLONDON}} for all London Stock Exchange holidays
  (actually, England holidays) in requested years.

}
\examples{
## UK Millenium day
specialHolidayGB(1999)     # as a dateTime object
specialHolidayGB(1999, "") # as a character string

## 2 special holidays in UK in 2022
specialHolidayGB(2022)     # [2022-06-03] [2022-09-19]
## what are their names?
specialHolidayGB(2022, named = TRUE)

## the Spring BH is usually on last Monday of May, but not in 2022
dayOfWeek(GBSpringBankHoliday(2020:2024))

## the above formed a nice 4-day weekend in early June 2022
## (look at the Thu-Fri sequence on 2-3 June)
dayOfWeek(holidayLONDON(2022))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
