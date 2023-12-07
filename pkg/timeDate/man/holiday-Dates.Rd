\name{holidayDate}
\alias{holidayDate}

\alias{Septuagesima}
\alias{Quinquagesima}
\alias{AshWednesday}
\alias{PalmSunday}
\alias{GoodFriday}
\alias{EasterSunday}
\alias{EasterMonday}
\alias{RogationSunday}
\alias{Ascension}
\alias{Pentecost}
\alias{PentecostMonday}
\alias{TrinitySunday}
\alias{CorpusChristi}
\alias{ChristTheKing}
\alias{Advent1st}
\alias{Advent2nd}
\alias{Advent3rd}
\alias{Advent4th}
\alias{ChristmasEve}
\alias{ChristmasDay}
\alias{BoxingDay}
\alias{NewYearsDay}
\alias{SolemnityOfMary}
\alias{Epiphany}
\alias{PresentationOfLord}
\alias{Annunciation}
\alias{TransfigurationOfLord}
\alias{AssumptionOfMary}
\alias{BirthOfVirginMary}
\alias{CelebrationOfHolyCross}
\alias{MassOfArchangels}
\alias{AllSaints}
\alias{AllSouls}
\alias{LaborDay}
\alias{CHBerchtoldsDay}
\alias{CHSechselaeuten}
\alias{CHAscension}
\alias{CHConfederationDay}
\alias{CHKnabenschiessen}

\alias{GBMayDay}  % deprecated
\alias{GBEarlyMayBankHoliday}

\alias{GBBankHoliday} % deprecated
\alias{GBSpringBankHoliday}

\alias{GBSummerBankHoliday}
\alias{GBMilleniumDay}
\alias{DEAscension}
\alias{DECorpusChristi}
\alias{DEGermanUnity}
\alias{DEChristmasEve}
\alias{DENewYearsEve}
\alias{FRFetDeLaVictoire1945}
\alias{FRAscension}
\alias{FRBastilleDay}
\alias{FRAssumptionVirginMary}
\alias{FRAllSaints}
\alias{FRArmisticeDay}
\alias{ITEpiphany}
\alias{ITLiberationDay}
\alias{ITAssumptionOfVirginMary}
\alias{ITAllSaints}
\alias{ITStAmrose}
\alias{ITImmaculateConception}
\alias{USDecorationMemorialDay}
\alias{USPresidentsDay}
\alias{USNewYearsDay}
\alias{USInaugurationDay}
\alias{USMLKingsBirthday}
\alias{USLincolnsBirthday}
\alias{USWashingtonsBirthday}
\alias{USMemorialDay}
\alias{USIndependenceDay}
\alias{USLaborDay}
\alias{USColumbusDay}
\alias{USElectionDay}
\alias{USVeteransDay}
\alias{USThanksgivingDay}
\alias{USChristmasDay}
\alias{USCPulaskisBirthday}
\alias{USGoodFriday}
\alias{USJuneteenthNationalIndependenceDay}
\alias{CAVictoriaDay}
\alias{CAFamilyDay}
\alias{CACanadaDay}
\alias{CACivicProvincialHoliday}
\alias{CALabourDay}
\alias{CAThanksgivingDay}
\alias{CaRemembranceDay}
\alias{JPVernalEquinox}
\alias{JPNewYearsDay}
\alias{JPGantan}
\alias{JPBankHolidayJan2}
\alias{JPBankHolidayJan3}
\alias{JPComingOfAgeDay}
\alias{JPSeijinNoHi}
\alias{JPNatFoundationDay}
\alias{JPKenkokuKinenNoHi}
\alias{JPGreeneryDay}
\alias{JPMidoriNoHi}
\alias{JPConstitutionDay}
\alias{JPKenpouKinenBi}
\alias{JPNationHoliday}
\alias{JPKokuminNoKyujitu}
\alias{JPChildrensDay}
\alias{JPKodomoNoHi}
\alias{JPMarineDay}
\alias{JPUmiNoHi}
\alias{JPRespectForTheAgedDay}
\alias{JPKeirouNOhi}  % 2023-11-27 deprecated, obvious mis-capitalisation
\alias{JPKeirouNoHi}  % 2023-11-27 new, replaces the above
\alias{JPMountainDay} % 2023-11-27 new
\alias{JPAutumnalEquinox}
\alias{JPShuubunNoHi}
\alias{JPHealthandSportsDay}
\alias{JPTaiikuNoHi}
\alias{JPNationalCultureDay}
\alias{JPBunkaNoHi}
\alias{JPThanksgivingDay}
\alias{JPKinrouKanshaNoHi}
\alias{JPEmperorsBirthday}
\alias{JPTennouTanjyouBi}
\alias{JPBankHolidayDec31}



\title{Public and ecclesiastical holidays}

\description{

  A collection of functions giving holiday dates in the G7 countries and
  Switzerland.

}


\usage{
Septuagesima(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

Quinquagesima(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

AshWednesday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

PalmSunday(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_include = "", \dots)

GoodFriday(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_include = "", \dots)

EasterSunday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

EasterMonday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

RogationSunday(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_include = "", \dots)

Ascension(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

Pentecost(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

PentecostMonday(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

TrinitySunday(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

CorpusChristi(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

ChristTheKing(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

Advent1st(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

Advent2nd(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

Advent3rd(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

Advent4th(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

ChristmasEve(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

ChristmasDay(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

BoxingDay(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

NewYearsDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

SolemnityOfMary(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

Epiphany(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_include = "", \dots)

PresentationOfLord(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

Annunciation(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

TransfigurationOfLord(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_include = "", \dots)

AssumptionOfMary(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_include = "", \dots)

BirthOfVirginMary(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

CelebrationOfHolyCross(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_include = "", \dots)

MassOfArchangels(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_include = "", \dots)

AllSaints(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

AllSouls(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_include = "", \dots)

LaborDay(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_include = "", \dots)

CHBerchtoldsDay(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

CHSechselaeuten(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

CHAscension(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

CHConfederationDay(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

CHKnabenschiessen(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

GBMayDay(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_include = "", \dots)

GBEarlyMayBankHoliday(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_include = "", \dots)

GBBankHoliday(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

GBSpringBankHoliday(year = getRmetricsOptions("currentYear"), 
                    value = "timeDate", na_include = "", \dots)

GBSummerBankHoliday(year = getRmetricsOptions("currentYear"), 
                    value = "timeDate", na_include = "", \dots)

GBMilleniumDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_include = "", \dots)

DEAscension(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

DECorpusChristi(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

DEGermanUnity(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

DEChristmasEve(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_include = "", \dots)

DENewYearsEve(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

FRFetDeLaVictoire1945(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_include = "", \dots)

FRAscension(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

FRBastilleDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

FRAssumptionVirginMary(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_include = "", \dots)

FRAllSaints(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

FRArmisticeDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_include = "", \dots)

ITEpiphany(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_include = "", \dots)

ITLiberationDay(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

ITAssumptionOfVirginMary(year = getRmetricsOptions("currentYear"), 
                         value = "timeDate", na_include = "", \dots)

ITAllSaints(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

ITStAmrose(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_include = "", \dots)

ITImmaculateConception(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_include = "", \dots)

USDecorationMemorialDay(year = getRmetricsOptions("currentYear"), 
                        value = "timeDate", na_include = "", \dots)

USPresidentsDay(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

USNewYearsDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

USInaugurationDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

USMLKingsBirthday(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

USLincolnsBirthday(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

USWashingtonsBirthday(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_include = "", \dots)

USMemorialDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

USIndependenceDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

USLaborDay(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_include = "", \dots)

USColumbusDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

USElectionDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

USVeteransDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

USThanksgivingDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

USChristmasDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_include = "", \dots)

USCPulaskisBirthday(year = getRmetricsOptions("currentYear"), 
                    value = "timeDate", na_include = "", \dots)

USGoodFriday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

USJuneteenthNationalIndependenceDay(
                                    year = getRmetricsOptions("currentYear"), 
                                    value = "timeDate", 
                                    na_include = "", \dots)

CAVictoriaDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

CACanadaDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

CACivicProvincialHoliday(year = getRmetricsOptions("currentYear"), 
                         value = "timeDate", na_include = "", \dots)

CALabourDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

CAThanksgivingDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

CaRemembranceDay(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_include = "", \dots)

JPVernalEquinox(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

JPNewYearsDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

JPGantan(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_include = "", \dots)

JPBankHolidayJan2(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

JPBankHolidayJan3(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

JPComingOfAgeDay(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_include = "", \dots)

JPSeijinNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

JPNatFoundationDay(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

JPKenkokuKinenNoHi(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

JPGreeneryDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

JPMidoriNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

JPConstitutionDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

JPKenpouKinenBi(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

JPNationHoliday(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_include = "", \dots)

JPKokuminNoKyujitu(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

JPChildrensDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_include = "", \dots)

JPKodomoNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

JPMarineDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

JPUmiNoHi(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_include = "", \dots)

JPRespectForTheAgedDay(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_include = "", \dots)

JPKeirouNOhi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

JPMountainDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

JPAutumnalEquinox(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

JPShuubunNoHi(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_include = "", \dots)

JPHealthandSportsDay(year = getRmetricsOptions("currentYear"), 
                     value = "timeDate", na_include = "", \dots)

JPTaiikuNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_include = "", \dots)

JPNationalCultureDay(year = getRmetricsOptions("currentYear"), 
                     value = "timeDate", na_include = "", \dots)

JPBunkaNoHi(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_include = "", \dots)

JPThanksgivingDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

JPKinrouKanshaNoHi(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

JPEmperorsBirthday(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)

JPTennouTanjyouBi(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_include = "", \dots)

JPBankHolidayDec31(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_include = "", \dots)
}


\arguments{

  \item{year}{

    an integer value or vector of year numbers including the
    century. These are integers of the form \code{CCYY}, e.g.
    \code{2000}.

  }

  \item{value}{

    the class of the returned value. If \code{"timeDate"}, the default,
    return a \code{"timeDate"} object, otherwise return a character
    vector.
    
  }
  \item{na_include}{

    how to treat \code{NA}s, \code{TRUE}, \code{FALSE} or a character
    string, see section \sQuote{Details}.

  }
  \item{...}{

    further arguments for \code{format.timeDate}, most notably
    \code{"format"}. Ignored if \code{value = "timeDate"}.

  }
    
}

\details{

  This help page discusses the public and ecclesiastical holidays per
  se. Some holidays fall by definition on a working day or a particular
  day of the week. For holidays that fall on weekends many countries
  have rules to declare a close by weekday a holiday. The functions here
  do not consider such issues but they are handled by the
  \code{holidayXXX} functions (e.g., \code{holidayLONDON}), see their
  help pages.

  Public holidays change over time as new ones are introduced, dropped
  or move to different days.  When a holiday date is requested for a
  year when it did not exist, what should be returned? The same question
  arises when the information is not available in this package.

  The ecclesiastical holidays are computed by traditional rules and
  in practice should be correct for all years.

  Traditionally, package \pkg{timeDate} was computing the dates of the
  holidays according to the current rules.  In versions of package
  \pkg{timeDate} greater than 4022.108 historical information was added
  for England and Japan holidays. The update functions return the dates
  according to the rules for the particular years.

  For future years the returned dates are always computed accoding to
  the current rules.

  For years before the first available rules, the default is to use
  those rules, whether the holiday existed or not.

  Argument \code{na.include} can be used to control this. If
  \code{na.include} is \code{TRUE}, the value for years when the holiday
  didn't exist will be \code{NA}. If \code{na.include} is \code{FALSE}
  an entry will not be incuded in the result at all. If it is a
  character string, the default, the closest available rules will be
  used.

  
  The deprecated \code{GBMayDay} and \code{GBBankHoliday} are still
  available but strongly discouraged. Instead, use
  \code{GBEarlyMayBankHoliday} and \code{GBSpringBankHoliday},
  respectively.

}

\note{

  The holiday information for most countries incomplete.  Contributions
  are welcome. Please include references for your sources.
  
}

\value{
  the date of the requested holiday as a \code{"timeDate"} object
}

\examples{
## CHSechselaeuten -
   # Sechselaeuten a half Day Bank Holiday in Switzerland
   CHSechselaeuten(2000:2010)
   CHSechselaeuten(getRmetricsOptions("currentYear"))

## German Unification Day:
   DEGermanUnity(getRmetricsOptions("currentYear"))
}

\keyword{chron}
