 
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

## 2023-11-27
##    GNB: reworked the Japan's holidays as they all had set to fixed dates.
##         and also to reflect changes in recent decades.

################################################################################
# FUNCTION:                 DESCRIPTION:
#  ...                       Holiday Dates
################################################################################

Septuagesima =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -63)
    timeDate(as.character(ans)) }

Quinquagesima =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -49)
    timeDate(as.character(ans)) }

AshWednesday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -46)
    timeDate(as.character(ans)) }

PalmSunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -7)
    timeDate(as.character(ans)) }

GoodFriday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -2)
    timeDate(as.character(ans)) }

EasterSunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year)
    timeDate(as.character(ans)) }

EasterMonday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 1)
    timeDate(as.character(ans)) }

RogationSunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 35)
    timeDate(as.character(ans)) }

Ascension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

Pentecost =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 49)
    timeDate(as.character(ans)) }

## GNB: WhitMonday
PentecostMonday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 50)
    timeDate(as.character(ans)) }

TrinitySunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 56)
    timeDate(as.character(ans)) }

CorpusChristi =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 60)
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

ChristTheKing =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 11, 20, 0)
    timeDate(as.character(ans)) }

Advent1st =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 11, 27, 0)
    timeDate(as.character(ans)) }

Advent2nd =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 12,  4, 0)
    timeDate(as.character(ans)) }

Advent3rd =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 12, 11, 0)
    timeDate(as.character(ans)) }

Advent4th =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 12, 18, 0)
    timeDate(as.character(ans)) }

ChristmasEve =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1224
    timeDate(as.character(ans)) }

ChristmasDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1225
    timeDate(as.character(ans)) }

BoxingDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1226
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

SolemnityOfMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }

Epiphany =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0106
    timeDate(as.character(ans)) }

PresentationOfLord =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0202
    timeDate(as.character(ans)) }

Annunciation =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0325
    timeDate(as.character(ans)) }

TransfigurationOfLord =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0806
    timeDate(as.character(ans)) }

AssumptionOfMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }

BirthOfVirginMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0908
    timeDate(as.character(ans)) }

CelebrationOfHolyCross =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0914
    timeDate(as.character(ans)) }

MassOfArchangels =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0929
    timeDate(as.character(ans)) }

AllSaints =
    function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }

AllSouls =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1102
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

NewYearsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }

LaborDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0501
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

CHBerchtoldsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0102
    timeDate(as.character(ans)) }

CHSechselaeuten =
function(year = getRmetricsOptions("currentYear")) {
    ans = NULL
    for (y in year) {
        theDate = .nth.of.nday(y, 4, 1, 3)
        if (as.character(theDate) == as.character(Easter(y, +1))) {
            theDate = .nth.of.nday(y, 4, 1, 4)
        }
        ans = c(ans, theDate)
    }
    timeDate(as.character(ans)) }

CHAscension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

CHConfederationDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0801
    timeDate(as.character(ans)) }

CHKnabenschiessen =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 9, 1, 2)
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

## GNB: Early May bank holiday
## GNB: renaming to the proper name; leaving for now 'GBMayDay' for compatibility
##
## the code in holidaysLONDON for y > 1978 was:
## if (y == 1995 || y == 2020) {
##     ## Was moved to May 8 to celebrate VE Day's 75th/50th anniversary
##     dts <- paste0(y, "-05-08")
##     holidays <- c(holidays, dts)
## } else {
##     lon <- timeDate(.on.or.after(y, 5, 1, 1), zone = "London",
##                     FinCenter = "Europe/London")
##     holidays <- c(holidays, as.character(lon))
## }
GBMayDay <- 
GBEarlyMayBankHoliday <- 
function(year = getRmetricsOptions("currentYear")) {
    ## TODO: y < 1978 -  NA or empty
    ## if (y >= 1978)
    ## # First Monday of May became a bank holiday
    
    ans = as.character(.nth.of.nday(year, 5, 1, 1))

    ## special: moved to May 8 to celebrate VE Day's 50th/75th anniversary
    ind <- year %in% c(1995, 2020)
    if(any(ind))
       ans[ind] <- paste0(year[ind], "0508") # not "-05-08" for consistency with 'ans'

    timeDate(ans)
}

## YC: Note GBBankHoliday returns Spring Bank Holiday
## GNB: renaming to a more common name; leaving for now 'GBBankHoliday' for compatibility
##
GBBankHoliday <- 
GBSpringBankHoliday <- 
function(year = getRmetricsOptions("currentYear")) {
    ## introduced in 1871, first on Whit Monday
        
    ans = as.character(.last.of.nday(year, 5, 31, 1))

    ## Whit Monday, which is exactly 50 days after Easter
    if(any(year <= 1964)) {
        ## using arg 'format' as the default for timeDate objects is "%Y-%m-%d"
        ans[year <= 1964] <- as.character(Easter(year[year <= 1964], 50), format = "%Y%m%d")
    }
    
    ## moved to be the day before XXX/Diamond/Platinum Jubilee bank holiday
    ind <- year %in% c(2002, 2012, 2022)
    if(any(ind)) {
        ans[year == 2002] <- "20020603"
        ans[year == 2012] <- "20120604" # Last Monday in May holiday moved to June 4, and
                                        # Queen's Diamond Jubilee on June 5

        ans[year == 2022] <- "20220602" # Thursday
    }
    timeDate(ans)
}

## GNB: added handling of historical dates before 1971,
##      including 1965-1970 for which many sources have it wrong for 1968, 1969
##
## Last Monday in August replaces first Monday in August as Summer BH.
##
## 2023-12-04 GNB:
##
## Between 1965-1970 there was a trial period of moving the Summer BH to 'the
##     end of the month', without formal definition (see wikipedia page). A
##     royal proclamation was issued each year for the exact dates. It seems
##     that they used the Monday after the weekend containing the last
##     Saturday in August.  in 1968 and 1969 Summer Bank Holiday is in the
##     beginning of Sep:
##     see  https://api.parliament.uk/historic-hansard/written-answers/1965/jun/03/bank-holidays-1967-and-1968
## https://family-law.co.uk/significance-bank-holidays/
##         2 Sep 1968
##         1 Sep 1969
##  TODO: add the above! But need to check reverse dependencies when doing that!
## 
## The formal definition as "Last Monday in August" is from the 1971 Act
## but the difference for 1965-1970 isa only for 1968, 1969 as noted above
GBSummerBankHoliday =
function(year = getRmetricsOptions("currentYear")) {
    ## established in 1871 as the 1st Monday in August, so
    ## ans[year <= 1870] <- NA or empty  :TODO:
    
    ans = .last.of.nday(year, 8, 31, 1) # year >= 1965
    if(any(year <= 1970)) {
        ## Summer BH on First Monday in August
        ans[year <= 1964] <- .on.or.after(year[year <= 1964], 8, 1, 1)

        ## correct for 1968 & 1969, see comments above
        ans[year == 1968] <- 19680902
        ans[year == 1969] <- 19690901
    }
    timeDate(as.character(ans))
}


## added by GNB
## TODO: it ignores, silently, the argument, not good?
GBMilleniumDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = 19991231
    timeDate(as.character(ans)) }

## TODO?
##
## GBOneOffBankHoliday =
## function(year = getRmetricsOptions("currentYear")) {
##     ans = 19991231
##     timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

DEAscension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

DECorpusChristi =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 60)
    timeDate(as.character(ans)) }

DEGermanUnity =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1003
    timeDate(as.character(ans)) }

DEChristmasEve =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1224
    timeDate(as.character(ans)) }

DENewYearsEve =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1231
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

FRFetDeLaVictoire1945 =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0508
    timeDate(as.character(ans)) }

FRAscension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

FRBastilleDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0714
    timeDate(as.character(ans)) }

FRAssumptionVirginMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }

FRAllSaints =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }

FRArmisticeDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------


ITEpiphany =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0106
    timeDate(as.character(ans)) }


ITLiberationDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0425
    timeDate(as.character(ans)) }


ITAssumptionOfVirginMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }


ITAllSaints =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }


ITStAmrose =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1207
    timeDate(as.character(ans)) }


ITImmaculateConception =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1208
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------


USNewYearsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }


USInaugurationDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0120
    timeDate(as.character(ans)) }


USMLKingsBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 1, 1, 3)
    timeDate(as.character(ans)) }


USLincolnsBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0212
    timeDate(as.character(ans)) }


USWashingtonsBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = year * 10000 + 222
    timeDate(as.character(ans)) }


USMemorialDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .last.of.nday(year, 5, 31, 1)
    timeDate(as.character(ans)) }


USIndependenceDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0704
    timeDate(as.character(ans)) }


USLaborDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) }


USColumbusDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) }


USElectionDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 11, 2, 2)
    timeDate(as.character(ans)) }


USVeteransDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }


USThanksgivingDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 11, 4, 4)
    timeDate(as.character(ans)) }


USChristmasDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1225
    timeDate(as.character(ans)) }


USCPulaskisBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 3, 1, 1)
    timeDate(as.character(ans)) }


USGoodFriday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -2)
    timeDate(as.character(ans)) }


USPresidentsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 2, 1, 3)
    timeDate(as.character(ans)) }


USDecorationMemorialDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0530
    timeDate(as.character(ans)) }

## Fixes issue #6755 by Ian E (ene100) 
## Juneteenth introduced as NAtional US holiday in 2021;
##    holiday on NYSE from 2022
## https://www.sec.gov/rules/sro/nyse/2021/34-93183.pdf (see p. 2 for definition)
USJuneteenthNationalIndependenceDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0619
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------


CAVictoriaDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.before(year, 5, 24, 1)
    timeDate(as.character(ans)) }


CAFamilyDay =
function(year = getRmetricsOptions("currentYear"))
{   # Description:
    #   Adds the new Family Day
    # Note:
    #   Check ...
    #   www.sbhlawyers.com/media/ELD%20Oct%2019%202007%20Public%20Holidays%20and%20Family%20Day.pdf
    #   Family Day will fall on the third Monday of
    #       every February, beginning in 2008.
    # Family Day:
    charvec = paste(year, "02", "01", sep = "-")
    ans = timeNthNdayInMonth(charvec, nday = 1, nth = 3)
    # Return Value:
    ans
}

CACanadaDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0701
    timeDate(as.character(ans)) }

CACivicProvincialHoliday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 8, 1, 1)
    timeDate(as.character(ans)) }

CALabourDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) }

CAThanksgivingDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) }

CaRemembranceDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------
# Japan
# ---------------------------------------------------------------------------- #

JPVernalEquinox <- function(year = getRmetricsOptions("currentYear")) {
    # Author:
    #   Parlamis Franklin wrote:
    #   It's me again, with Japanese calendar minutiae I'm sure you've all
    #   been dying to brush up on. The fCalendar functions don't include
    #   the Japanese Vernal Equinox holiday. this is perhaps because there
    #   is no easy way to calculate it. at any rate, here's a function I
    #   wrote to fill the gap.

    # Notes:
    #   Origin and End Date data from
    #   http://aa.usno.navy.mil/data/docs/EarthSeasons.html
    #   The function Vernal.Equinox delivers correct values at the
    #   endpoints of the above data. There may be minor variances
    #   (+/- a few minutes) in the intermediate values, because the
    #   function linearly approximates a phenomenon that is apparently
    #   nonlinear in recorded time.

    Equinox.Origin <- timeCalendar(1992, 3, 20, 8, 48, 0, FinCenter = "GMT")
    Data.EndDate <- timeCalendar(2020, 3, 20, 3, 49, 0, FinCenter = "GMT")
    Total.Seconds <- as.numeric(Data.EndDate-Equinox.Origin)*24*60*60
    Mean.Annual.Seconds <- Total.Seconds / (atoms(Data.EndDate)$Y -
        atoms(Equinox.Origin)$Y)
    Vernal.Equinox <- function(year)
    {
        Equinox.Origin +
        unclass((year-atoms(Equinox.Origin)$Y)*Mean.Annual.Seconds)
    }

    # Nota bene: JP Vernal Equinox is celebrated when the equinox
    #   occurs in the Japanese time zone (see, e.g., 2006, where GMT
    #   Vernal Equinox is on 20 March, but Japanese Equinox holiday is
    #   21 March)

    # Return Value:
    trunc(timeDate(as.character(Vernal.Equinox(year)), FinCenter = "Tokyo"))
}

JPNewYearsDay <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans))
}

JPGantan <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans))
}

JPBankHolidayJan2 <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0102
    timeDate(as.character(ans))
}

JPBankHolidayJan3 <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0103
    timeDate(as.character(ans))
}

## GNB - fixed to give 2nd Monday of January
##       Also, combined JPSeijinNoHi and JPComingOfAgeDay, since they seem to
##       be the same thing.
JPSeijinNoHi <-
JPComingOfAgeDay <- function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 1, 1, 2) # 2023-11-27 was: year*10000 + 0115
    timeDate(as.character(ans))
}

JPKenkokuKinenNoHi <-
JPNatFoundationDay <- function(year = getRmetricsOptions("currentYear")) {
    ans =year*10000 + 0211   # GNB: ok, fixed date
    timeDate(as.character(ans))
}

JPMidoriNoHi <-
JPGreeneryDay <- function(year = getRmetricsOptions("currentYear")) {
    ans <- year*10000 + 0504 # year >= 2007
    if(any(year < 2007)) {
        ans[year < 2007] <- year*10000 + 0429
        ## ans[year < 1989] <- NA # but it was Emperor's Birthday back then
    }
    
    timeDate(as.character(ans))
}

## https://en.wikipedia.org/wiki/Golden_Week_(Japan)
##  Citizen's Holiday (国民の休日, Kokumin no Kyūjitsu) is a generic term for any official
##  holiday. Until 2006, 4 May was an unnamed but official holiday because of a rule that
##  converts any day between two holidays into a new holiday.
JPKokuminNoKyujitu <-
JPNationHoliday <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0504
    timeDate(as.character(ans))
}

        
JPKenpouKinenBi <-
JPConstitutionDay <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0503
    timeDate(as.character(ans))
}

JPKodomoNoHi <-
JPChildrensDay <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0505
    timeDate(as.character(ans))
}

## https://en.wikipedia.org/wiki/Marine_Day
## 2023 date: July 17
## 2022 date: July 18
## 2024 date: July 15
## 2025 date: July 21
JPUmiNoHi <-
JPMarineDay <- function(year = getRmetricsOptions("currentYear")) {
    ## GNB was: ans = year*10000 + 0720
    ans = .nth.of.nday(year, 7, 1, 3) # from 2003
    ans[year < 2003] <- year*10000 + 0720
    ## ans[year < 1996] <- NA
    if(any(year %in% c(2020, 2021))) {
        ans[year == 2020] <- year*10000 + 0723  # Olympics
        ans[year == 2021] <- year*10000 + 0722
    }
    
    timeDate(as.character(ans))
}

## 2023-11-27 - deprecated;
JPKeirouNOhi <- function(year = getRmetricsOptions("currentYear")) {
    .Deprecated("JPKeirouNoHi")
    JPKeirouNoHi(year)
}

JPKeirouNoHi <-
JPRespectForTheAgedDay <- function(year = getRmetricsOptions("currentYear")) {
    ## GNB was: ans = year*10000 + 0915
    ## https://en.wikipedia.org/wiki/Respect_for_the_Aged_Day
    ans = .nth.of.nday(year, 9, 1, 3)
    if(any(year < 2003)) {
        ans[year < 2003] = year*10000 + 0915
        ## if(any(year < 1966))
        ##     ans[year < 1966] <- NA   # TODO: do others return NA? or simply empty timeDate?
    }
    
    timeDate(as.character(ans))
}



## GNB: TODO: this cannot be constant, see above for the vernal equinox.
##
##      for now, changing it to 23 Sep and correct for known dates when it is on other dates,
##      usually 22 Sep https://en.wikipedia.org/wiki/Autumnal_Equinox_Day
JPShuubunNoHi <-
JPAutumnalEquinox <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0923  # was + 0924 but this is rare
    on22nd <- c(2016, 2024,2028) # incomplete; after 2024 predicted
    timeDate(as.character(ans))
}

JPTaiikuNoHi <- 
JPHealthandSportsDay <- function(year = getRmetricsOptions("currentYear")) {
    ## GNB was: ans = year*10000 + 1010
    ## https://en.wikipedia.org/wiki/Sports_Day_(Japan)
    ans = .nth.of.nday(year, 10, 1, 2)
    ans[year == 2020] <-  year*10000 + 0724  # Olympics openning, but that was cancelled.
    ans[year == 2021] <-  year*10000 + 0723  # Olympics openning, but that was cancelled.
    if(any(year < 2000)) {
        ans[1966 <= year & year <= 1999] <-  year*10000 + 1010
    }
    timeDate(as.character(ans))
}

JPBunkaNoHi <-
JPNationalCultureDay <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1103
    timeDate(as.character(ans))
}

JPKinrouKanshaNoHi <-
JPThanksgivingDay <- function(year = getRmetricsOptions("currentYear")) {
    ## https://en.wikipedia.org/wiki/Labor_Thanksgiving_Day
    ans = year*10000 + 1123   ## TODO: if on Sunday move to Monday  !!!
    timeDate(as.character(ans))
}

## GNB: none in 2019;
##   2020+ - 23 Feb (Naruhito), but seems that if its on Sunday (on the weekend?), it is moved to Monday.
##       for 2025 - 24 Feb,
##                  according to https://www.officeholidays.com/holidays/japan/the-emperors-birthday
##   1989-2018     23 Dec (Akihito)
##   1926(7?)-1988 29 April (stil public holiday - Greenery Day since 1989, Showa day since 2007.
##                 https://en.wikipedia.org/wiki/The_Emperor%27s_Birthday
JPTennouTanjyouBi  <-
JPEmperorsBirthday <- function(year = getRmetricsOptions("currentYear")) {
    ## GNB was: ans = year*10000 + 1123
    ans = year*10000 + 0223 # Naruhito
    if(any(year < 2020)){
        ## ans[year == 2019] <- NA
        ans[1989 <= year & year <= 2018] <- year*10000 + 1223 # Akihito
        ans[1927 <= year & year <= 1988] <- year*10000 + 0429 # Showa
        ## ans[year <  1927] <- NA                               # TODO ?
    }
    
    timeDate(as.character(ans))
}

## TODO: check
JPBankHolidayDec31 <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1231
    timeDate(as.character(ans))
}

## GNB new 2023-11-27 its absence reported by Sylvie Lachaud
## Moutain day 11/08/2023 : is it missing in your list ?
## https://en.wikipedia.org/wiki/Mountain_Day#Mountain_Day_in_Japan
JPMountainDay <- function(year = getRmetricsOptions("currentYear")) {
    ans <- year*10000 + 0811
    ans[year == 2020] <-  year*10000 + 0810  # Olympics openning (cancelled)
    ans[year == 2021] <-  year*10000 + 0809  # Olympics openning
    ## TODO: doesn't exist before 2016
    ## if(any(year < 2016))
    ##     ans[year < 2016] <- NA
    timeDate(as.character(ans))
}
