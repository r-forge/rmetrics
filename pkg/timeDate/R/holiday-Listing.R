
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


################################################################################
# FUNCTION:                 DESCRIPTION:
#  listHolidays              Lists Holidays
################################################################################

listHolidays <- function(pattern = ".*") {
    ## A function implemented by Diethelm Wuertz
    ## Consolidated, refactored and amended by Georgi N. Boshnakov

    ## Description:
    ##   Returns all public and ecclestical holidays

    if (pattern == "*") pattern = "\\\\*"

    ## no need for sort() anymore as .all_holidays() is sorted
    ##    TODO: not explicitly stated in R docs that grep() returns indices
    ##          in increasing order.
    as.character(.all_holidays[grep(pattern = pattern, x = .all_holidays)])
}

.all_holidays <- sort(c(
    "Septuagesima",
    "Quinquagesima",
    "AshWednesday",
    "PalmSunday",
    "GoodFriday",
    "Easter",
    "EasterSunday",
    "EasterMonday",
    "RogationSunday",
    "Ascension",
    "Pentecost",
    "PentecostMonday",
    "TrinitySunday",
    "CorpusChristi",
    "ChristTheKing",
    "Advent1st",
    "Advent2nd",
    "Advent3rd",
    "Advent4th",
    "ChristmasEve",
    "ChristmasDay",
    "BoxingDay",
    "NewYearsDay",
    "SolemnityOfMary",
    "Epiphany",
    "PresentationOfLord",
    "Annunciation",
    "TransfigurationOfLord",
    "AssumptionOfMary",
    "BirthOfVirginMary",
    "CelebrationOfHolyCross",
    "MassOfArchangels",
    "AllSaints",
    "AllSouls",
    "LaborDay",
    "CHBerchtoldsDay",
    "CHSechselaeuten",
    "CHAscension",
    "CHConfederationDay",
    "CHKnabenschiessen",
    "GBEarlyMayBankHoliday", # was: "GBMayDay",
    "GBSpringBankHoliday",   # was: "GBBankHoliday"
    "GBSummerBankHoliday",
    "specialHolidayGB", # new 2023-12-08
    "DEAscension",
    "DECorpusChristi",
    "DEGermanUnity",
    "DEChristmasEve",
    "DENewYearsEve",
    "FRFetDeLaVictoire1945",
    "FRAscension",
    "FRBastilleDay",
    "FRAssumptionVirginMary",
    "FRAllSaints",
    "FRArmisticeDay",
    "ITEpiphany",
    "ITLiberationDay",
    "ITAssumptionOfVirginMary",
    "ITAllSaints",
    "ITStAmrose",
    "ITImmaculateConception",
    "USDecorationMemorialDay",
    "USPresidentsDay",
    "USNewYearsDay",
    "USInaugurationDay",
    "USMLKingsBirthday",
    "USLincolnsBirthday",
    "USWashingtonsBirthday",
    "USMemorialDay",
    "USIndependenceDay",
    "USLaborDay",
    "USColumbusDay",
    "USElectionDay",
    "USVeteransDay",
    "USThanksgivingDay",
    "USChristmasDay",
    "USCPulaskisBirthday",
    "USGoodFriday",
    "USJuneteenthNationalIndependenceDay",
    "CAVictoriaDay",
    "CACanadaDay",
    "CAFamilyDay",
    "CACivicProvincialHoliday",
    "CALabourDay",
    "CAThanksgivingDay",
    "CaRemembranceDay",
    "JPVernalEquinox",
    "JPNewYearsDay",
    "JPGantan",
    "JPBankHolidayJan2",
    "JPBankHolidayJan3",
    "JPComingOfAgeDay",
    "JPSeijinNoHi",
    "JPNatFoundationDay",
    "JPKenkokuKinenNoHi",
    "JPGreeneryDay",
    "JPMidoriNoHi",
    "JPConstitutionDay",
    "JPKenpouKinenBi",
    "JPNationHoliday",
    "JPKokuminNoKyujitu",
    "JPChildrensDay",
    "JPKodomoNoHi",
    "JPMarineDay",
    "JPUmiNoHi",
    "JPRespectForTheAgedDay",
    "JPKeirouNoHi",
    "JPMountainDay",
    "JPAutumnalEquinox",
    "JPShuubunNoHi",
    "JPHealthandSportsDay",
    "JPTaiikuNoHi",
    "JPNationalCultureDay",
    "JPBunkaNoHi",
    "JPThanksgivingDay",
    "JPKinrouKanshaNoHi",
    "JPEmperorsBirthday",
    "JPTennouTanjyouBi",
    "JPBankHolidayDec31",

    "InternationalWomensDay"   # 2024-09-16 new
    ))

################################################################################
