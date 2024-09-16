
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
##    GNB: reworked the Japan's holidays as they all were set to fixed dates,
##         and also to reflect changes in recent decades.

################################################################################
# FUNCTION:                 DESCRIPTION:
#  ...                       Holiday Dates
################################################################################

## 2023-12-07 GNB: new
format..sdate <- function(x, format = "-", ...) {
    ## TODO: think it over
    if(format == "-")
        paste(substr(x, 1, 4), substr(x, 5, 6), substr(x, 7, 8), sep = "-")
    else if(format == "")
        paste(substr(x, 1, 4), substr(x, 5, 6), substr(x, 7, 8), sep = "")
    else # TODO: needs more here
        format.default(x, format = format, ...)
}

Septuagesima =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, -63)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

Quinquagesima =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, -49)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

AshWednesday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, -46)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

PalmSunday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, -7)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

GoodFriday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, -2)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

EasterSunday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

EasterMonday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 1)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

RogationSunday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 35)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

Ascension =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 39)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

Pentecost =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 49)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

## GNB: WhitMonday
PentecostMonday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 50)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

TrinitySunday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 56)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

CorpusChristi =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 60)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}


# ------------------------------------------------------------------------------

ChristTheKing =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.after(year, 11, 20, 0)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

Advent1st =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.after(year, 11, 27, 0)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

Advent2nd =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.after(year, 12,  4, 0)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

Advent3rd =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.after(year, 12, 11, 0)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

Advent4th =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.after(year, 12, 18, 0)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

ChristmasEve =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1224
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

ChristmasDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1225
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

BoxingDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1226
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------

SolemnityOfMary =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

Epiphany =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0106
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

PresentationOfLord =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0202
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

Annunciation =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0325
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

TransfigurationOfLord =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0806
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

AssumptionOfMary =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0815
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

BirthOfVirginMary =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0908
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

CelebrationOfHolyCross =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0914
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

MassOfArchangels =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0929
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

AllSaints =
    function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

AllSouls =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1102
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------

NewYearsDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

LaborDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0501
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------

CHBerchtoldsDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0102
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

CHSechselaeuten =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = NULL
    for (y in year) {
        theDate = .nth.of.nday(y, 4, 1, 3)
        if (as.character(theDate) == as.character(Easter(y, +1))) {
            theDate = .nth.of.nday(y, 4, 1, 4)
        }
        ans = c(ans, theDate) # this changes the class from .sdate to numeric,
                              # hence as.character() below
    }
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

CHAscension =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 39)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

CHConfederationDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0801
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

CHKnabenschiessen =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 9, 1, 2)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


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
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## TODO: y < 1978 -  NA or empty
    ## if (y >= 1978)
    ## # First Monday of May became a bank holiday
    if(flag <- isTRUE(na_drop))
        year <- year[year >= 1978]

    ans = .nth.of.nday(year, 5, 1, 1)

    ## special: moved to May 8 to celebrate VE Day's 50th/75th anniversary
    ind <- year %in% c(1995, 2020)
    if(any(ind))
       ans[ind] <- 10000 * year[ind] +  0508

    if(!flag && isFALSE(na_drop))
        ans[year < 1978] <- NA

    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

## YC: Note GBBankHoliday returns Spring Bank Holiday
## GNB: renaming to a more common name; leaving for now 'GBBankHoliday' for compatibility
##
GBBankHoliday <-
GBSpringBankHoliday <-
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## introduced in 1871, first on Whit Monday

    if((flag <- isTRUE(na_drop)) && any(year < 1871))
        year <- year[year >= 1871]

    ans = .last.of.nday(year, 5, 31, 1)

    ## Whit Monday, which is exactly 50 days after Easter
    if(any(year <= 1964)) {
        ## using arg 'format' as the default for timeDate objects  is "%Y-%m-%d"
        ans[year <= 1964] <- as.numeric(format(Easter(year[year <= 1964], 50),
                                               format = "%Y%m%d"))
    }

    ## moved to be the day before XXX/Diamond/Platinum Jubilee bank holiday
    ind <- year %in% c(2002, 2012, 2022)
    if(any(ind)) {
        ans[year == 2002] <- 20020603
        ans[year == 2012] <- 20120604  # Last Monday in May holiday moved to June 4, and
                                        # Queen's Diamond Jubilee on June 5

        ans[year == 2022] <- 20220602  # Thursday
    }

    if(!flag && isFALSE(na_drop))
        ans[year < 1871] <- NA

    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
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
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## established in 1871 as the 1st Monday in August, so
    ## ans[year < 1871] <- NA or empty  :TODO:
    ## didn't exist before 1871
    if((flag <- isTRUE(na_drop)) && any(year < 1871))
        year <- year[year >= 1871]


    ans = .last.of.nday(year, 8, 31, 1) # year >= 1965
    if(any(year <= 1970)) {
        ## Summer BH on First Monday in August
        ans[year <= 1964] <- .on.or.after(year[year <= 1964], 8, 1, 1)

        ## correct for 1968 & 1969, see comments above
        ans[year == 1968] <- 19680902
        ans[year == 1969] <- 19690901
    }
    if(!flag && isFALSE(na_drop))
        ans[year < 1871] <- NA

    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


## added by GNB
## TODO: it ignores, silently, the argument, not good?
GBMilleniumDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    .Deprecated("Use specialHolidayGB(1999)")
    ans = 19991231
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

.gb_oneoff_holidays <- c(
    "GBRoyalWeddingDay1981"        , "1981-07-29", # Royal wedding
    "GBMillenumDay"                , "1999-12-31", # UK millenum day

    "GBQueensGoldenJubileeDay"     , "2002-06-04", # Spring BH moved to June 3,
                                                   # and Queen's Jubilee on June 4

    "GBRoyalWeddingDay2011"        , "2011-04-29", # Royal wedding

    "GBQueensDiamondJubilee"       , "2012-06-05", # Early May BH moved to June 4
                                                   # and Queen's Diamond Jubilee on June 5

                                                   # Spring BH moved to June 2
    "GBQueensPlatinumJubileeDay"   , "2022-06-03", # Friday, Platinum Jubilee BH
    "GBQueensFuneralDay"           , "2022-09-19", # BH, State Funeral Queen Elizabeth II
    "GBKingCharlesIIICoronationDay", "2023-05-08"  # BH, coronation of King Charles III
  )

## .gb_oneoff_BH <- matrix(.gb_oneoff_holidays, ncol = 2, byrow = TRUE)
## .gb_years <- as.integer(substr(.gb_oneoff_BH[ , 2], 1, 4))
## .unique_gb_years <- unique(.gb_years)

.GByears_with_oneoff <-
    c(1981, 1999, 2002, 2011, 2012, 2022, 2023)

specialHolidayGB <-
function(year  = getRmetricsOptions("currentYear"), value = "timeDate", named = FALSE, ...) {
    ## TODO: option to return  names for the holidays;
    ##       maybe it is better to just provide separate function for that
    ##
    ## TODO: No, it doesn't make sense to return NA's. In some years there may be
    ##       more than one special holiday (2022 in GB), so alignment cannot be guaranteed.
    ##       Hence:
    ##          1. use sort and unique on year as in the holidayXXX functions
    ##          2. remove the irrelevant arguments
    ##          3. rename this function, e.g. to holidaySpecialGB
    ##
    ## here value = "" doesn't make sense, treat it the same as FALSE

    year <- year[year %in% .GByears_with_oneoff]

    ans <- character(0)

    ## TODO: need more elegant solution but this should do.
    ##       Bear in mind that there may be more than one special BH in a year!
    if(named) {
        ## same as for 'else' with names
        for(y in year) {
            ans <- list(ans,
                 if (y == 1981)               # Royal wedding was a public holiday
                    c("GBRoyalWeddingDay1981" = "1981-07-29")
                else if (y == 1999)          # UK millenum day
                    c(GBMillenumDay = "1999-12-31")
                else if (y == 2002)          # Last Monday in May holiday moved to June
                    c(GBQueensGoldenJubileeDay = "2002-06-04") # 3, and Queen's Jubilee on June 4
                else if (y == 2011)       # Royal wedding declared a public holiday
                    c(GBRoyalWeddingDay2011 = "2011-04-29")
                else if (y == 2012)
                    c(GBQueensDiamondJubilee = "2012-06-05") # Last Monday in May holiday moved to June 4, and
                                          # Queen's Diamond Jubilee on June 5
                else if (y == 2022)
                             # Last Monday in May (i.e., Spring Bank Holiday) moved to June 2
                             # Unique Bank holidays:  Queen's Diamond Jubilee.
                             #                        State Funeral of Queen Elizabeth II
                    c(# paste0(y, "-06-02"), # Thursday, Spring bank holiday
                      GBQueensPlatinumJubileeDay = "2022-06-03",     # Friday, Platinum Jubilee bank holiday
                      GBQueensFuneralDay = "2022-09-19")     # BH for the State Funeral of Queen Elizabeth II
                else if (y == 2023)     # Bank holiday for the coronation of King Charles III
                    c(GBKingCharlesIIICoronationDay = "2023-05-08")
                else
                    stop("should not reach this branch of the program")
                )
        }
    } else {
        for(y in year) {
            ans <- list(ans,
                 if (y == 1981)               # Royal wedding was a public holiday
                    "1981-07-29"
                else if (y == 1999)          # UK millenum day
                    "1999-12-31"
                else if (y == 2002)          # Last Monday in May holiday moved to June
                    "2002-06-04"             # 3, and Queen's Jubilee on June 4
                else if (y == 2011)       # Royal wedding declared a public holiday
                    "2011-04-29"
                else if (y == 2012)
                    "2012-06-05"          # Last Monday in May holiday moved to June 4, and
                                          # Queen's Diamond Jubilee on June 5
                else if (y == 2022)
                             # Last Monday in May (i.e., Spring Bank Holiday) moved to June 2
                             # Unique Bank holidays:  Queen's Diamond Jubilee.
                             #                        State Funeral of Queen Elizabeth II
                    c(# paste0(y, "-06-02"), # Thursday, Spring bank holiday
                      "2022-06-03",     # Friday, Platinum Jubilee bank holiday
                      "2022-09-19")     # BH for the State Funeral of Queen Elizabeth II
                else if (y == 2023)     # Bank holiday for the coronation of King Charles III
                    "2023-05-08"
                else
                    stop("should not reach this branch of the program")
                )
        }
    }

    ans <- sort(unlist(ans))

    if(value == "timeDate") timeDate(ans, format = "%Y-%m-%d")  else ans
}


# ------------------------------------------------------------------------------

DEAscension =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 39)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

DECorpusChristi =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 60)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

DEGermanUnity =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1003
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

DEChristmasEve =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1224
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

DENewYearsEve =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1231
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------

FRFetDeLaVictoire1945 =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0508
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

FRAscension =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, 39)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

FRBastilleDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0714
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}

FRAssumptionVirginMary =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0815
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

FRAllSaints =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

FRArmisticeDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1111
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------


ITEpiphany =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0106
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


ITLiberationDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0425
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


ITAssumptionOfVirginMary =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0815
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


ITAllSaints =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


ITStAmrose =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1207
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


ITImmaculateConception =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1208
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------


USNewYearsDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USInaugurationDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0120
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USMLKingsBirthday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 1, 1, 3)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USLincolnsBirthday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0212
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USWashingtonsBirthday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year * 10000 + 222
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USMemorialDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .last.of.nday(year, 5, 31, 1)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USIndependenceDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0704
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USLaborDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 9, 1, 1)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USColumbusDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 10, 1, 2)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USElectionDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.after(year, 11, 2, 2)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USVeteransDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1111
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USThanksgivingDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 11, 4, 4)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USChristmasDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1225
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


USCPulaskisBirthday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 3, 1, 1)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USGoodFriday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = Easter(year, -2)
    if(value == "timeDate") timeDate(as.character(ans))  else as.character(ans, ...)
}


USPresidentsDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 2, 1, 3)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


USDecorationMemorialDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0530
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## Fixes issue #6755 by Ian E (ene100)
## Juneteenth introduced as National US holiday in 2021;
##    holiday on NYSE from 2022
## https://www.sec.gov/rules/sro/nyse/2021/34-93183.pdf (see p. 2 for definition)
USJuneteenthNationalIndependenceDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## didn't exist before 2021
    if(flag <- isTRUE(na_drop))
        year <- year[year >= 2021]

    ans <- year*10000 + 0619
    if(!flag && isFALSE(na_drop))
        ans[year < 2021] <- NA

    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------


CAVictoriaDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .on.or.before(year, 5, 24, 1)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}


CAFamilyDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...)
{   # Description:
    #   Adds the new Family Day
    # Note:
    #   Check ...
    #   www.sbhlawyers.com/media/ELD%20Oct%2019%202007%20Public%20Holidays%20and%20Family%20Day.pdf
    #   Family Day will fall on the third Monday of
    #       every February, beginning in 2008.
    # Family Day:

    ## rewritten by GNB

    ## doesn't exist before 2008
    if(flag <- isTRUE(na_drop))
        year <- year[year >= 2008]

    ans <- .nth.of.nday(year, 2, 1, 3)

    if(!flag && isFALSE(na_drop))
        ans[year < 2008] <- NA

    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

CACanadaDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0701
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

CACivicProvincialHoliday =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 8, 1, 1)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

CALabourDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 9, 1, 1)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

CAThanksgivingDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 10, 1, 2)
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

CaRemembranceDay =
function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1111
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


# ------------------------------------------------------------------------------
# Japan
# ---------------------------------------------------------------------------- #

JPVernalEquinox <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
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

JPNewYearsDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPGantan <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0101
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPBankHolidayJan2 <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0102
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPBankHolidayJan3 <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0103
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## GNB - fixed to give 2nd Monday of January
##       Also, combined JPSeijinNoHi and JPComingOfAgeDay, since they seem to
##       be the same thing.
JPSeijinNoHi <-
JPComingOfAgeDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = .nth.of.nday(year, 1, 1, 2) # 2023-11-27 was: year*10000 + 0115
    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

JPKenkokuKinenNoHi <-
JPNatFoundationDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans <- year * 10000 + 0211
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPMidoriNoHi <-
JPGreeneryDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans <- year*10000 + 0504 # year >= 2007
    if(any(year < 2007)) {
        ans[year < 2007] <- year*10000 + 0429
        ## ans[year < 1989] <- NA # but it was Emperor's Birthday back then
    }

    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## https://en.wikipedia.org/wiki/Golden_Week_(Japan)
##  Citizen's Holiday (国民の休日, Kokumin no Kyūjitsu) is a generic term for any official
##  holiday. Until 2006, 4 May was an unnamed but official holiday because of a rule that
##  converts any day between two holidays into a new holiday.
JPKokuminNoKyujitu <-
JPNationHoliday <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0504
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


JPKenpouKinenBi <-
JPConstitutionDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0503
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPKodomoNoHi <-
JPChildrensDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0505
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## https://en.wikipedia.org/wiki/Marine_Day
## 2023 date: July 17
## 2022 date: July 18
## 2024 date: July 15
## 2025 date: July 21
JPUmiNoHi <-
JPMarineDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## GNB was: ans = year*10000 + 0720
    if(flag <- isTRUE(na_drop))
        year <- year[year >= 1996]

    ans = .nth.of.nday(year, 7, 1, 3) # from 2003
    ans[year < 2003] <- year*10000 + 0720
    if(!flag && isFALSE(na_drop))
        ans[year < 1996] <- NA

    if(any(year %in% c(2020, 2021))) {
        ans[year == 2020] <- 2020 * 10000 + 0723  # Olympics
        ans[year == 2021] <- 2021 * 10000 + 0722
    }

    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

## 2023-11-27 - deprecated;
JPKeirouNOhi <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    .Deprecated("JPKeirouNoHi")
    JPKeirouNoHi(year)
}

JPKeirouNoHi <-
JPRespectForTheAgedDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## GNB was: ans = year*10000 + 0915
    ## https://en.wikipedia.org/wiki/Respect_for_the_Aged_Day
    if(flag <- isTRUE(na_drop))
        year <- year[year >= 1966]

    ans = .nth.of.nday(year, 9, 1, 3)
    if(any(year < 2003)) {
        ans[year < 2003] = year*10000 + 0915
        if(!flag && isFALSE(na_drop))
            ans[year < 1966] <- NA
    }

    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

## GNB: TODO: this cannot be constant, see above for the vernal equinox.
##
##      for now, changing it to 23 Sep and correct for known dates when it is on other dates,
##      usually 22 Sep https://en.wikipedia.org/wiki/Autumnal_Equinox_Day
JPShuubunNoHi <-
JPAutumnalEquinox <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans <- year * 10000 + 0923  # was + 0924 but this is rare
    on22nd <- c(2016, 2024, 2028) # incomplete; after 2024 predicted
    ans[year %in% on22nd] <- year[year %in% on22nd] * 10000 + 0922

    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPTaiikuNoHi <-
JPHealthandSportsDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## updated and rewritten by GNB
    ##   was: ans = year*10000 + 1010
    ## https://en.wikipedia.org/wiki/Sports_Day_(Japan)
    ans = .nth.of.nday(year, 10, 1, 2)

    ans[year == 2021] <-  2021 * 10000 + 0723 # Olympics openning
    ans[year == 2020] <-  2020 * 10000 + 0724 # Olympics cancelled but holiday stayed

    if(length(tmp_flag <- 1966 <= year & year <= 1999))
        ans[tmp_flag] <-  year[tmp_flag] * 10000 + 1010

    if(value == "timeDate") timeDate(ans)  else format..sdate(ans, ...)
}

JPBunkaNoHi <-
JPNationalCultureDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1103
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

JPKinrouKanshaNoHi <-
JPThanksgivingDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## https://en.wikipedia.org/wiki/Labor_Thanksgiving_Day
    ans = year*10000 + 1123   ## TODO: if on Sunday move to Monday  !!!
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## GNB: none in 2019;
##   2020+ - 23 Feb (Naruhito), but seems that if its on Sunday (on the weekend?), it is moved to Monday.
##       for 2025 - 24 Feb,
##                  according to https://www.officeholidays.com/holidays/japan/the-emperors-birthday
##   1989-2018     23 Dec (Akihito)
##   1926(7?)-1988 29 April (stil public holiday - Greenery Day since 1989, Showa day since 2007.
##                 https://en.wikipedia.org/wiki/The_Emperor%27s_Birthday
JPTennouTanjyouBi  <-
JPEmperorsBirthday <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    if(flag <- isTRUE(na_drop)) {
        year <- year[year >=  1927]   # don't know what it was before 1927
        year <- year[year !=  2019]   # due to abdication this holiday was cancelled in 2019
    }

    ans = year*10000 + 0223 # Naruhito

    if(any(year < 2020)){
        if(!flag && isFALSE(na_drop)) {
            ans[year == 2019] <- NA
            ans[year <= 1927] <- NA
        }

        flag_1989to2018 <- 1989 <= year & year <= 2018
        flag_1927to1988 <- 1927 <= year & year <= 1988

        if(length(flag_1989to2018))
            ans[flag_1989to2018] <- year[flag_1989to2018] * 10000 + 1223 # Akihito
        if(length(flag_1927to1988))
            ans[flag_1927to1988] <- year[flag_1927to1988] * 10000 + 0429 # Showa
        ## TODO: before 1927 ?
    }

    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## TODO: check
JPBankHolidayDec31 <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 1231
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}

## GNB new 2023-11-27 its absence reported by Sylvie Lachaud
## Moutain day 11/08/2023 : is it missing in your list ?
## https://en.wikipedia.org/wiki/Mountain_Day#Mountain_Day_in_Japan
JPMountainDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ## didn't exist before 2016
    if(flag <- isTRUE(na_drop))
        year <- year[year >= 2016]

    ans <- year * 10000 + 0811
    if(!flag && isFALSE(na_drop))
        ans[year < 2016] <- NA

    ans[year == 2020] <-  2020*10000 + 0810  # Olympics openning (cancelled)
    ans[year == 2021] <-  2021*10000 + 0809  # Olympics openning

    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}


## Other holidays
InternationalWomensDay <- function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
    ans = year*10000 + 0308
    if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
}
