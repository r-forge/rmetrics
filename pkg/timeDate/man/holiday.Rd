\name{holiday}
\alias{holiday}


\title{Holiday dates}

\description{
  Returns the date of a holiday. 
}

\usage{
holiday(year = getRmetricsOptions("currentYear"), Holiday = "Easter", 
        \dots, names = FALSE)
}

\arguments{
  \item{Holiday}{
    one or more names of holidays 
    as a character vector or a list of the unquoted function names.

  }
  \item{year}{
    an integer value or vector of years, formatted as \code{YYYY}.
  }
  \item{...}{further arguments, currently ignored.}
  \item{names}{if \code{TRUE} include holiday names.}
}

\value{
  an object of class \code{"timeDate"}
}

\details{

  The package contains functions for most ecclesiastical and public
  holidays in the G7 countries and Switzerland.
  
  Easter is the central ecclesiastical holiday. Many other holidays are
  related to this feast. The function \code{Easter} computes the dates
  of Easter and related ecclesiastical holidays for the requested year
  vector.

  \code{holiday} calculates the dates of ecclesiastical or publich
  holidays in the G7 countries, e.g.  \code{holiday(2003, "GoodFriday")}.

  Rmetrics contains holiday functions automatically loaded at startup
  time. The user can add easily additional holiday functions. 

  If \code{names} is TRUE the returned dates will be named. If
  \code{Holiday} is named, its names are used. Otherwise, if
  \code{Holiday} is a character vector, it is used for the names. If the
  holidays are specified as unquoted symbols, those are turned into
  strings for the names. As a lost resort, if the names cannot be
  inferred, generated names are used, see the examples. Note that the
  names will be duplicated, unless \code{length(year) == 1}.

}

\section{Overview}{
    The following ecclesiastical and public \code{[HOLIDAY]} functions 
    in the G7 countries and Switzerland are available: \cr
    
    
    \emph{Holidays Related to Easter:} 

    Septuagesima, Quinquagesima, AshWednesday, PalmSunday,
    GoodFriday,  EasterSunday, Easter, EasterMonday, 
    RogationSunday, Ascension, Pentecost, PentecostMonday, 
    TrinitySunday CorpusChristi. \cr 
     
    
    \emph{Holidays Related to Christmas:} 

    ChristTheKing, Advent1st, Advent1st, Advent3rd, 
    Advent4th, ChristmasEve, ChristmasDay, BoxingDay, 
    NewYearsDay. \cr
    
    
    \emph{Other Ecclestical Feasts:} 

    SolemnityOfMary, Epiphany, PresentationOfLord, 
    Annunciation, TransfigurationOfLord, AssumptionOfMary, 
    AssumptionOfMary, BirthOfVirginMary, CelebrationOfHolyCross, 
    MassOfArchangels, AllSaints, AllSouls. \cr
    
    
    \emph{CHZurich - Public Holidays:} 

    CHBerchtoldsDay, CHSechselaeuten, CHAscension, 
    CHConfederationDay, CHKnabenschiessen. \cr
    
    
    \emph{GBLondon - Public Holidays:} 

    GBEarlyMayBankHoliday, GBSpringBankHoliday
    GBSummerBankHoliday, GBNewYearsEve.

    (GBMayDay and GBBankHoliday have been removed. Use
    GBEarlyMayBankHoliday and GBSpringBankHoliday, respectively)
    \cr

    
    \emph{DEFrankfurt - Public Holidays:} 

    DEAscension, DECorpusChristi, DEGermanUnity, DEChristmasEve,
    DENewYearsEve. \cr
    
    
    \emph{FRParis - Public Holidays:} 
    
    FRFetDeLaVictoire1945, FRAscension, FRBastilleDay, 
    FRAssumptionVirginMary, FRAllSaints, FRArmisticeDay. \cr
    
    
    \emph{ITMilano - Public Holidays:} 

    ITEpiphany, ITLiberationDay, ITRepublicAnniversary, 
    ITAssumptionOfVirginMary, ITAllSaints, ITWWIVictoryAnniversary, 
    ITStAmrose, ITImmaculateConception. \cr
    
    
    \emph{USNewYork/USChicago - Public Holidays:} 

    USNewYearsDay, USInaugurationDay, USMLKingsBirthday, 
    USLincolnsBirthday, USWashingtonsBirthday, USMemorialDay, 
    USIndependenceDay, USLaborDay,  USColumbusDay, USElectionDay, 
    USVeteransDay, USThanksgivingDay, USChristmasDay, 
    USCPulaskisBirthday, USGoodFriday,
    USJuneteenthNationalIndependenceDay. \cr 
    
    
    \emph{CAToronto/CAMontreal - Public Holidays:} 

    CAVictoriaDay, CACanadaDay, CACivicProvincialHoliday, 
    CALabourDay, CAThanksgivingDay, CaRemembranceDay. \cr
    
    
    \emph{JPTokyo/JPOsaka - Public Holidays:} 

    JPNewYearsDay, JPGantan, JPBankHolidayJan2, JPBankHolidayJan3,
    JPComingOfAgeDay, JPSeijinNoHi, JPNatFoundationDay,
    JPKenkokuKinenNoHi, JPGreeneryDay, JPMidoriNoHi, 
    JPConstitutionDay, JPKenpouKinenBi, JPNationHoliday, 
    JPKokuminNoKyujitu, JPChildrensDay, JPKodomoNoHi, 
    JPMarineDay, JPUmiNoHi, JPRespectForTheAgedDay,
    JPKeirouNoHi, JPAutumnalEquinox, JPShuubun-no-hi, 
    JPHealthandSportsDay, JPTaiikuNoHi, JPNationalCultureDay, 
    JPBunkaNoHi, JPThanksgivingDay, JPKinrouKanshaNohi, 
    JPKinrou-kansha-no-hi, JPEmperorsBirthday,
    JPTennou-tanjyou-bi, JPTennou-tanjyou-bi. \cr
    JPMountainDay
}

\seealso{
  \code{\link{BoxingDay}}, etc., for descriptions of the individual
  holiday functions,

  \code{\link{listHolidays}} for a list (character vector) of all
  holidays,

  \code{\link{Easter}},

  \code{\link{holidayLONDON}},
  \code{\link{holidayNERC}},
  \code{\link{holidayNYSE}},
  \code{\link{holidayTSX}},
  \code{\link{holidayZURICH}} for holidays at major financial centers.
}

\examples{
## holiday
# Dates for GoodFriday from 2000 until 2005:
holiday(2000:2005, "GoodFriday")                  
holiday(2000:2005, GoodFriday) # same (GoodFriday is a function)

# Good Friday and Easter
holiday(2000:2005, c("GoodFriday", "Easter"))                  
holiday(2000:2005, c(GoodFriday, Easter))                  
   
## Easter   
Easter(2000:2005)

## GoodFriday
GoodFriday(2000:2005)
Easter(2000:2005, -2) 

#### Named holidays

## single year
holiday(2025, Holiday = "Easter", names = TRUE)
holiday(2025, Holiday = Easter, names = TRUE)

## more years
holiday(2024:2025, Holiday = "Easter", names = TRUE)
holiday(2024:2025, Holiday = Easter, names = TRUE)

## more than one holiday
holiday(2024:2025, Holiday = c("Easter", "ChristmasDay"), names = TRUE)
holiday(2024:2025, Holiday = c(Easter, ChristmasDay), names = TRUE)

## holidays in a variable containing character values
ho1 <- "Easter"
holiday(2025, Holiday = ho1, names = TRUE)


## here, Easter is evaluated and holiday() doesn't see the name
ho1a <- Easter
ho3 <- c(Easter, GBEarlyMayBankHoliday)

holiday(2025, Holiday = ho1a, names = TRUE)

## if you wish/need to use a separate variable, use 'quote':
ho1b <- quote(Easter)
ho3b <- quote(c(Easter, GBEarlyMayBankHoliday))

holiday(2025, Holiday = ho1b, names = TRUE)
holiday(2025, Holiday = ho3b, names = TRUE)
holiday(2024:2025, Holiday = ho3b, names = TRUE)

## with string values there is no need to quote:
ho2 <- c("Easter", "GBEarlyMayBankHoliday")
holiday(2025, Holiday = ho2, names = TRUE)




holiday(Holiday = c(Easter, GBEarlyMayBankHoliday), names = TRUE)


holiday(2024:2025, Holiday = tmp2, names = TRUE)
holiday(2024:2025, Holiday = tmp1a, names = TRUE)
holiday(2024:2025, Holiday = tmp1, names = TRUE)


## redundant c() but no harm done
holiday(2024:2025, Holiday = c("Easter"), names = TRUE)
holiday(2024:2025, Holiday = c(Easter), names = TRUE)

}

\keyword{chron}
