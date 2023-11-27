
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

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Septuagesima =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -63)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Quinquagesima =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -49)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
AshWednesday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -46)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
PalmSunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -7)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
GoodFriday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -2)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
EasterSunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
EasterMonday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
RogationSunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 35)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Ascension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Pentecost =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 49)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
PentecostMonday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 50)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
TrinitySunday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 56)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CorpusChristi =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 60)
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ChristTheKing =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 11, 20, 0)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Advent1st =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 11, 27, 0)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Advent2nd =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 12,  4, 0)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Advent3rd =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 12, 11, 0)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Advent4th =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 12, 18, 0)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ChristmasEve =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1224
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ChristmasDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1225
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
BoxingDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1226
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
SolemnityOfMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Epiphany =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0106
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
PresentationOfLord =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0202
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
Annunciation =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0325
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
TransfigurationOfLord =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0806
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
AssumptionOfMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
BirthOfVirginMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0908
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CelebrationOfHolyCross =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0914
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
MassOfArchangels =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0929
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
AllSaints =
    function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
AllSouls =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1102
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
NewYearsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
LaborDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0501
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CHBerchtoldsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0102
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
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

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CHAscension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CHConfederationDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0801
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CHKnabenschiessen =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 9, 1, 2)
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
## GNB: Early May bank holiday
# GNB: renaming to the proper name; leaving for now 'GBMayDay' for compatibility
GBEarlyMayBankHoliday <- 
function(year = getRmetricsOptions("currentYear")) {
    ans = as.character(.nth.of.nday(year, 5, 1, 1))

    ## special: moved to May 8 to celebrate VE Day's 50th/75th anniversary
    ind <- year %in% c(1995, 2020)

    if(any(ind))
       ans[ind] <- paste0(year[ind], "0508") # not "-05-08" for consistency with 'ans'

    timeDate(ans)
}

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
# YC: Note GBBankHoliday returns Spring Bank Holiday
# GNB: renaming to the proper name; leaving for now 'GBBankHoliday' for compatibility
GBSpringBankHoliday <- 
function(year = getRmetricsOptions("currentYear")) {
    ans = as.character(.last.of.nday(year, 5, 31, 1))
    
    ## moved to be the day before XXX/Diamond/Platinum Jubilee bank holiday
    ind <- year %in% c(2002, 2012, 2022)
    if(any(ind)) {
        ans[year == 2002] <- "20020603"
        ans[year == 2012] <- "20120604"
        ans[year == 2022] <- "20220602" # Thursday
    }
    
    timeDate(ans)
}

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
GBSummerBankHoliday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .last.of.nday(year, 8, 31, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
GBMilleniumDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = 19991231
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
DEAscension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
DECorpusChristi =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 60)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
DEGermanUnity =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1003
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
DEChristmasEve =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1224
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
DENewYearsEve =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1231
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
FRFetDeLaVictoire1945 =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0508
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
FRAscension =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, 39)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
FRBastilleDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0714
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
FRAssumptionVirginMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
FRAllSaints =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
FRArmisticeDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ITEpiphany =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0106
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ITLiberationDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0425
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ITAssumptionOfVirginMary =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0815
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ITAllSaints =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1101
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ITStAmrose =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1207
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
ITImmaculateConception =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1208
    timeDate(as.character(ans)) }


# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USNewYearsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0101
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USInaugurationDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0120
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USMLKingsBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 1, 1, 3)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USLincolnsBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0212
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USWashingtonsBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = year * 10000 + 222
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USMemorialDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .last.of.nday(year, 5, 31, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USIndependenceDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0704
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USLaborDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USColumbusDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USElectionDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.after(year, 11, 2, 2)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USVeteransDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1111
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USThanksgivingDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 11, 4, 4)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USChristmasDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 1225
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USCPulaskisBirthday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 3, 1, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USGoodFriday =
function(year = getRmetricsOptions("currentYear")) {
    ans = Easter(year, -2)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USPresidentsDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 2, 1, 3)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USDecorationMemorialDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0530
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
## Fixes issue #6755 by Ian E (ene100) 
## Juneteenth introduced as NAtional US holiday in 2021;
##    holiday on NYSE from 2022
## https://www.sec.gov/rules/sro/nyse/2021/34-93183.pdf (see p. 2 for definition)
##
## Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
USJuneteenthNationalIndependenceDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0619
    timeDate(as.character(ans)) }

# ------------------------------------------------------------------------------

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CAVictoriaDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .on.or.before(year, 5, 24, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
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

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CACanadaDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0701
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CACivicProvincialHoliday =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 8, 1, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CALabourDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 9, 1, 1)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
CAThanksgivingDay =
function(year = getRmetricsOptions("currentYear")) {
    ans = .nth.of.nday(year, 10, 1, 2)
    timeDate(as.character(ans)) }

# ---------------------------------------------------------------------------- #
# Roxygen Tags
#' @export
# ---------------------------------------------------------------------------- #
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
        ans[year < 1989] <- NA # but it was Emperor's Birthday back then
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
    ans[year < 1996] <- NA
    if(any(year %in% c(2020, 2021))) {
        ans[year == 2020] <- year*10000 + 0723  # Olympics
        ans[year == 2021] <- year*10000 + 0722
    }
    
    timeDate(as.character(ans))
}

JPKeirouNOhi <-
JPRespectForTheAgedDay <- function(year = getRmetricsOptions("currentYear")) {
    ## GNB was: ans = year*10000 + 0915
    ## https://en.wikipedia.org/wiki/Respect_for_the_Aged_Day
    ans = .nth.of.nday(year, 9, 1, 3)
    if(any(year < 2003)) {
        ans[year < 2003] = year*10000 + 0915
        if(any(year < 1966))
            ans[year < 1966] <- NA
    }
    
    timeDate(as.character(ans))
}



## GNB: TODO: this cannot be constant, see above for the vernal equinox.
JPShuubunNoHi <-
JPAutumnalEquinox <- function(year = getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0924
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
        ans[year == 2019] <- NA
        ans[1989 <= year & year <= 2018] <- year*10000 + 1223 # Akihito
        ans[1927 <= year & year <= 1988] <- year*10000 + 0429 # Showa
        ans[year <  1927] <- NA                               # TODO ?
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
    ans[year == 2020] <-  year*10000 + 0810  # Olympics openning, but that was cancelled.
    ans[year == 2021] <-  year*10000 + 0809  # Olympics openning, but that was cancelled.
    if(any(year < 2016))
        ans[year < 2016] <- NA
    timeDate(as.character(ans))
}

################################################################################
## GNB: compatibility

GBMayDay <- GBEarlyMayBankHoliday
GBBankHoliday <- GBSpringBankHoliday
