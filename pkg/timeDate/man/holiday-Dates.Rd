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
\alias{GBMilleniumDay} % deprecated

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

\alias{InternationalWomensDay}


\title{Public and ecclesiastical holidays}

\description{

  A collection of functions giving holiday dates in the G7 countries and
  Switzerland.

}


\usage{
Septuagesima(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

Quinquagesima(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

AshWednesday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

PalmSunday(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_drop = TRUE, \dots)

GoodFriday(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_drop = TRUE, \dots)

EasterSunday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

EasterMonday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

RogationSunday(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_drop = TRUE, \dots)

Ascension(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

Pentecost(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

PentecostMonday(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

TrinitySunday(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

CorpusChristi(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

ChristTheKing(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

Advent1st(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

Advent2nd(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

Advent3rd(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

Advent4th(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

ChristmasEve(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

ChristmasDay(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

BoxingDay(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

NewYearsDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

SolemnityOfMary(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

Epiphany(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_drop = TRUE, \dots)

PresentationOfLord(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

Annunciation(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

TransfigurationOfLord(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_drop = TRUE, \dots)

AssumptionOfMary(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_drop = TRUE, \dots)

BirthOfVirginMary(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

CelebrationOfHolyCross(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_drop = TRUE, \dots)

MassOfArchangels(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_drop = TRUE, \dots)

AllSaints(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

AllSouls(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_drop = TRUE, \dots)

LaborDay(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_drop = TRUE, \dots)

CHBerchtoldsDay(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

CHSechselaeuten(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

CHAscension(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

CHConfederationDay(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

CHKnabenschiessen(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

GBMayDay(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_drop = TRUE, \dots)

GBEarlyMayBankHoliday(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_drop = TRUE, \dots)

GBBankHoliday(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

GBSpringBankHoliday(year = getRmetricsOptions("currentYear"), 
                    value = "timeDate", na_drop = TRUE, \dots)

GBSummerBankHoliday(year = getRmetricsOptions("currentYear"), 
                    value = "timeDate", na_drop = TRUE, \dots)

GBMilleniumDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_drop = TRUE, \dots)

DEAscension(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

DECorpusChristi(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

DEGermanUnity(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

DEChristmasEve(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_drop = TRUE, \dots)

DENewYearsEve(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

FRFetDeLaVictoire1945(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_drop = TRUE, \dots)

FRAscension(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

FRBastilleDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

FRAssumptionVirginMary(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_drop = TRUE, \dots)

FRAllSaints(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

FRArmisticeDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_drop = TRUE, \dots)

ITEpiphany(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_drop = TRUE, \dots)

ITLiberationDay(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

ITAssumptionOfVirginMary(year = getRmetricsOptions("currentYear"), 
                         value = "timeDate", na_drop = TRUE, \dots)

ITAllSaints(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

ITStAmrose(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_drop = TRUE, \dots)

ITImmaculateConception(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_drop = TRUE, \dots)

USDecorationMemorialDay(year = getRmetricsOptions("currentYear"), 
                        value = "timeDate", na_drop = TRUE, \dots)

USPresidentsDay(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

USNewYearsDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

USInaugurationDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

USMLKingsBirthday(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

USLincolnsBirthday(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

USWashingtonsBirthday(year = getRmetricsOptions("currentYear"), 
                      value = "timeDate", na_drop = TRUE, \dots)

USMemorialDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

USIndependenceDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

USLaborDay(year = getRmetricsOptions("currentYear"), 
           value = "timeDate", na_drop = TRUE, \dots)

USColumbusDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

USElectionDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

USVeteransDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

USThanksgivingDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

USChristmasDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_drop = TRUE, \dots)

USCPulaskisBirthday(year = getRmetricsOptions("currentYear"), 
                    value = "timeDate", na_drop = TRUE, \dots)

USGoodFriday(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

USJuneteenthNationalIndependenceDay(
                                    year = getRmetricsOptions("currentYear"), 
                                    value = "timeDate", na_drop = TRUE, 
                                    \dots)

CAVictoriaDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

CACanadaDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

CACivicProvincialHoliday(year = getRmetricsOptions("currentYear"), 
                         value = "timeDate", na_drop = TRUE, \dots)

CALabourDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

CAThanksgivingDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

CaRemembranceDay(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_drop = TRUE, \dots)

JPVernalEquinox(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

JPNewYearsDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

JPGantan(year = getRmetricsOptions("currentYear"), value = "timeDate", 
         na_drop = TRUE, \dots)

JPBankHolidayJan2(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

JPBankHolidayJan3(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

JPComingOfAgeDay(year = getRmetricsOptions("currentYear"), 
                 value = "timeDate", na_drop = TRUE, \dots)

JPSeijinNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

JPNatFoundationDay(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

JPKenkokuKinenNoHi(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

JPGreeneryDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

JPMidoriNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

JPConstitutionDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

JPKenpouKinenBi(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

JPNationHoliday(year = getRmetricsOptions("currentYear"), 
                value = "timeDate", na_drop = TRUE, \dots)

JPKokuminNoKyujitu(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

JPChildrensDay(year = getRmetricsOptions("currentYear"), 
               value = "timeDate", na_drop = TRUE, \dots)

JPKodomoNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

JPMarineDay(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

JPUmiNoHi(year = getRmetricsOptions("currentYear"), value = "timeDate", 
          na_drop = TRUE, \dots)

JPRespectForTheAgedDay(year = getRmetricsOptions("currentYear"), 
                       value = "timeDate", na_drop = TRUE, \dots)

JPKeirouNOhi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

JPMountainDay(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

JPAutumnalEquinox(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

JPShuubunNoHi(year = getRmetricsOptions("currentYear"), 
              value = "timeDate", na_drop = TRUE, \dots)

JPHealthandSportsDay(year = getRmetricsOptions("currentYear"), 
                     value = "timeDate", na_drop = TRUE, \dots)

JPTaiikuNoHi(year = getRmetricsOptions("currentYear"), 
             value = "timeDate", na_drop = TRUE, \dots)

JPNationalCultureDay(year = getRmetricsOptions("currentYear"), 
                     value = "timeDate", na_drop = TRUE, \dots)

JPBunkaNoHi(year = getRmetricsOptions("currentYear"), 
            value = "timeDate", na_drop = TRUE, \dots)

JPThanksgivingDay(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

JPKinrouKanshaNoHi(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

JPEmperorsBirthday(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

JPTennouTanjyouBi(year = getRmetricsOptions("currentYear"), 
                  value = "timeDate", na_drop = TRUE, \dots)

JPBankHolidayDec31(year = getRmetricsOptions("currentYear"), 
                   value = "timeDate", na_drop = TRUE, \dots)

InternationalWomensDay(year = getRmetricsOptions("currentYear"),
                       value = "timeDate", na_drop = TRUE, ...)}


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
  \item{na_drop}{

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
  for England and Japan holidays. The updated functions return the dates
  according to the rules for the particular years.

  For future years the returned dates are always computed accoding to
  the current rules.

  For years before the first available rules, the default is to use
  those rules, whether the holiday existed or not.

  Argument \code{na_drop} can be used to control this. If \code{na_drop}
  is \code{TRUE}, an entry will not be incuded in the result at all.  If
  \code{na_drop} is \code{FALSE} the value for years when the holiday
  didn't exist will be \code{NA}.  If it is a character string, the
  default, the closest available rules will be used.

  Not all functions respect argument \code{na_drop}. In that case they
  act as if \code{na_drop} is a character string.

  
  The deprecated \code{GBMayDay} and \code{GBBankHoliday} are still
  available but strongly discouraged. Instead, use
  \code{GBEarlyMayBankHoliday} and \code{GBSpringBankHoliday},
  respectively.

}

\note{

  The holiday information for most countries is incomplete.
  Contributions are welcome. Please include references for your sources,
  whenever possible.
  
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
