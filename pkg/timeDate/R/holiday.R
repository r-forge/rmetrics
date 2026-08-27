
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
#  holiday                   Returns a holiday date of G7 and CH
################################################################################




## $$a = \text{година} \pmod{19}$$ 
## $$b = \text{година} \pmod 4$$ 
## $$c = \text{година} \pmod 7$$ 
## $$d = (19 \cdot a + 15) \pmod{30}$$ 
## $$e = (2 \cdot b + 4 \cdot c + 6 \cdot d + 6) \pmod 7$$

## Полученият резултат за сумата d + e ни дава датата по Юлианския календар:
## Ако d + e < 22, Великден е на дата (22 + d + e) март (по стар стил).
## Ако d + e ≥ 22, Великден е на дата (d + e - 9) април (по стар стил).
## 
## За да преминем към гражданския (Новоюлиански) календар за нашия век (XX и XXI в.), към
## получената дата просто добавяме 13 дни.

## todo: crude but should work OEaster() for now
.len_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

OEaster1 <- function(year = getRmetricsOptions("currentYear"), shift = 0) {
    ## length(year) == 1
    a <- year %% 19
    b <- year %%  4
    c <- year %%  7

    d <- (19 * a + 15) %% 30
    e <- (2 * b + 4 * c + 6 * d + 6) %% 7
    dpe <- d + e  # <= 29 + 6 = 35

    century <- year %/% 100 - 16
    greg <- 10 + century - century %/% 4
    
    if(dpe < 22) {
        month <- 3 # march
        day <- dpe + 22
        if(day > 31) {
            month <- 4 # april
            day <- day - 31
        }
    } else { # dpe >= 22
        month <- 4 # april
        day <- dpe - 9
    }

    ## convert the Julian date to Gregorian date
    day <- day + greg
    while(day > .len_month[month]) {
        day <- day - .len_month[month]
        month <- month + 1
        ## todo: check for month > 12
    }

    #browser()
    timeDate(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))
}

OEaster <- function(year = getRmetricsOptions("currentYear"), shift = 0) {
    a <- year %% 19
    b <- year %%  4
    c <- year %%  7

    d <- (19 * a + 15) %% 30
    e <- (2 * b + 4 * c + 6 * d + 6) %% 7
    dpe <- d + e  # <= 29 + 6 = 35

    century <- year %/% 100 - 16
    greg <- 10 + century - century %/% 4

    ## if(dpe < 22) {
    ##     month <- 3 # march
    ##     day <- dpe + 22
    ##     if(day > 31) {
    ##         month <- 4 # april
    ##         day <- day - 31
    ##     }
    ## } else { # dpe >= 22
    ##     month <- 4 # april
    ##     day <- dpe - 9
    ## }

    flags <- dpe < 22
    month <- ifelse(flags, 3, 4)
    day   <- dpe + ifelse(flags, 22, -9)

    ## convert the Julian date to Gregorian date and add shift
    day <- day + greg + shift
    
    if(any(day > .len_month[month])) {
        ind <- which(day > .len_month[month])
        day[ind] <- day[ind] - .len_month[month[ind]]
        month[ind] <- month[ind] + 1
        ## todo: check for month > 12
    }

    #browser()
    timeDate(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))
}

## August 2026, GNB
##   put holiday() in a local environment
##   give it class "holiday"
##   define $-method for that class to enable holiday$US, holiday$BG, etc.

holiday <- local({

    .md <- 1111
    holiday_from_date <- 
        function(year = getRmetricsOptions("currentYear"), value = "timeDate", na_drop = TRUE, ...) {
            ans = year*10000 + .md
            if(value == "timeDate") timeDate(as.character(ans))  else format..sdate(ans, ...)
        }

    ## Note this sees the Western Easter; the copy in orthodox sees OEaster
    ##   TODO: do it more cleanly?
    shift <- 0
    Easter_shift <- function(year = getRmetricsOptions("currentYear"))
        Easter(year, shift = shift)
        
    Septuagesima = -63
    Quinquagesima = -49
    AshWednesday = -46
    PalmSunday = -7
    
    GoodFriday = -2
    ##EasterSunday
    EasterMonday = 1
    RogationSunday = 35
    Ascension = 39
    Pentecost = 49
    ## GNB: WhitMonday
    PentecostMonday = 50
    TrinitySunday = 56
    CorpusChristi = 60
    
    ## ------------------------------------------------------------------------------

    ## ChristTheKing =
    ## Advent1st =
    ## Advent2nd =
    ## Advent3rd =
    ## Advent4th =

    ChristmasEve = 1224
    ChristmasDay = 1225
    BoxingDay = 1226

    ## ------------------------------------------------------------------------------

    SolemnityOfMary = 0101
    Epiphany = 0106
    PresentationOfLord = 0202
    Annunciation = 0325
    TransfigurationOfLord = 0806
    AssumptionOfMary = 0815
    BirthOfVirginMary = 0908
    CelebrationOfHolyCross = 0914
    MassOfArchangels = 0929
    AllSaints = 1101
    AllSouls = 1102

        NewYearsDay <- 0101
        LaborDay <- 0501
        




    Orthodox <- local({
        GoodFriday <- -2
        
        Easter <- OEaster

        ChristmasDay <- 1225

        
        ##Septuagesima <- -63 # not observed by Orthodox
        ##Quinquagesima <- -49 # not observed by Orthodox
        #AshWednesday <- -46 # not observed in Orthodox
        PalmSunday <- -7 # Tsvetnitsa
        
        GoodFriday <- -2 # Razpeti Petak
        ##EasterSunday
        EasterMonday <- 1
        RogationSunday <- NA_integer_ # West: 35; not in  Orthodox
        Ascension <- 39 # Spasov Den, Vaznesenie Gospodne
        Pentecost <- HolyTrinitySunday <- 49 # also: Holy Trinity Day; NOTE: different from Catholic Trinity, see below
        ## GNB: WhitMonday
        PentecostMonday <- 50
        
        TrinitySunday <- NA_integer_ # West:  56 but Orthodox celebrates this on Pentecost, see HolyTrinitySunday
        CorpusChristi <- NA_integer_ # West: 60 # absent in Orthodox

        ## these are not observed in Orthodox
        ##
        ## ChristTheKing # 
        ## Advent1st # 
        ## Advent2nd # 
        ## Advent3rd # 
        ## Advent4th # 
        
        ChristmasEve <- 1224
        ChristmasDay <- 1225
        BoxingDay <- 1226
        
        ## ------------------------------------------------------------------------------
        
        ## SolemnityOfMary <- 0101  # other days to celebrate Virgin Mary
        Epiphany <- 0106 # Yordanov den, Voditsi, Bogoyavlenie
        PresentationOfLord <- 0202 # Sretenie Gospodne
        Annunciation <- 0325 # Blagovestenie
        TransfigurationOfLord <- 0806
        AssumptionOfMary2 <- 0815 # Uspenie Bogorodichno, Golyama Bogoroditsa
        BirthOfVirginMary <- 0908  # Malka Bogoroditsa
        CelebrationOfHolyCross <- 0914 # Krastov den
        
        MassOfArchangels <- NA_integer_ # Orthodox analogue is ArchangelDay; BG -> ArchangelovDen <- 1108
        ArchangelDay <- 1108
        
        AllSaints <- 56 # PetroviZagovezni;   West: 1101 but movable in Orthodox
        AllSouls <- NA_integer_   # West: 1102 but in Orthodox there are many Saturdays for this

        shift <- 0
        Easter_shift <- function(year = getRmetricsOptions("currentYear"))
            Easter(year, shift = shift)
        
        Julian <- local({

            ChristmasDay <- 0107

    
            
            structure(
                function(year, Holiday = "Easter", ..., names = FALSE) {
                    "Julian() curently does nothing"
                },
                class = "holiday"
            )
        })

        structure(
            function(year, Holiday = "Easter", ..., names = FALSE) {
                "orthodox() curently does nothing"
            },
            class = "holiday"
        )
    })

    
    
    function(year = getRmetricsOptions("currentYear"), Holiday = "Easter", ..., names = FALSE) {
    # A function implemented by Diethelm Wuertz
    # Significantly modified and amended by GNB

    # Description:
    #   Returns the date of a holiday, year may be a vector.

    # Arguments:
    #   year - an integer variable or vector for the year(s) ISO-8601
    #       formatted as "CCYY" as integers.
    #   holiday - a character string naming the holiday. By default
    #       "Easter". Allowable names are the holidays in the G7
    #       countries and Switzerland.

    # Value:
    #   Returns the date of a listed holiday for the selected
    #   "year"(s), an object of class 'timeDate'.

    # Example:
    #   holiday()
    #   holiday(2000:2009, "USLaborDay")
    #   class(holiday())

    # List of Valid Holiday Character Strings:
    #   The following ecclestial and public holidays in
    #       the G7 countries and Switzerland are available:
    #   Holidays Related to Easter:
    #       Septuagesima, Quinquagesima, AshWednesday, PalmSunday,
    #       GoodFriday,  EasterSunday, Easter, EasterMonday,
    #       RogationSunday, Ascension, Pentecost, PentecostMonday,
    #       TrinitySunday CorpusChristi.
    #   Holidays Related to Christmas:
    #       ChristTheKing, Advent1st, Advent1st, Advent3rd,
    #       Advent4th, ChristmasEve, ChristmasDay, BoxingDay,
    #       NewYearsDay.
    #   Other Ecclestical Feasts:
    #       SolemnityOfMary, Epiphany, PresentationOfLord,
    #       Annunciation, TransfigurationOfLord, AssumptionOfMary,
    #       AssumptionOfMary, BirthOfVirginMary, CelebrationOfHolyCross,
    #       MassOfArchangels, AllSaints, AllSouls.
    #   CHZurich - Public Holidays:
    #       CHBerchtoldsDay, CHSechselaeuten, CHAscension,
    #       CHConfederationDay, CHKnabenschiessen.
    #   GBLondon - Public Holidays:
    #       GBMayDay, GBBankHoliday, GBSummerBankHoliday,
    #       GBNewYearsEve.
    #   DEFrankfurt - Public Holidays:
    #       DEAscension, DECorpusChristi, DEGermanUnity, DEChristmasEve,
    #       DENewYearsEve.
    #   FRParis - Public Holidays:
    #       FRFetDeLaVictoire1945, FRAscension, FRBastilleDay,
    #       FRAssumptionVirginMary, FRAllSaints, FRArmisticeDay.
    #   ITMilano - Public Holidays:
    #       ITEpiphany, ITLiberationDay, ITRepublicAnniversary,
    #       ITAssumptionOfVirginMary, ITAllSaints, ITWWIVictoryAnniversary,
    #       ITStAmrose, ITImmaculateConception.
    #   USNewYork/USChicago - Public Holidays:
    #       USNewYearsDay, USInaugurationDay, USMLKingsBirthday,
    #       USLincolnsBirthday, USWashingtonsBirthday, USMemorialDay,
    #       USIndependenceDay, USLaborDay,  USColumbusDay, USElectionDay,
    #       USVeteransDay, USThanksgivingDay, USChristmasDay,
    #       USCPulaskisBirthday, USGoodFriday.
    #   CAToronto/CAMontreal - Public Holidays:
    #       CAVictoriaDay, CACanadaDay, CACivicProvincialHoliday,
    #       CALabourDay, CAThanksgivingDay, CaRemembranceDay.
    #   JPTokyo/JPOsaka - Public Holidays:
    #       JPNewYearsDay, JPGantan, JPBankHolidayJan2, JPBankHolidayJan3,
    #       JPComingOfAgeDay, JPSeijinNoHi, JPNatFoundationDay,
    #       JPKenkokuKinenNoHi, JPGreeneryDay, JPMidoriNoHi,
    #       JPConstitutionDay, JPKenpouKinenBi, JPNationHoliday,
    #       JPKokuminNoKyujitu, JPChildrensDay, JPKodomoNoHi,
    #       JPMarineDay, JPUmiNoHi, JPRespectForTheAgedDay,
    #       JPKeirouNoHi, JPAutumnalEquinox, JPShuubun-no-hi,
    #       JPHealthandSportsDay, JPTaiikuNoHi, JPNationalCultureDay,
    #       JPBunkaNoHi, JPThanksgivingDay, JPKinrouKanshaNohi,
    #       JPKinrou-kansha-no-hi, JPEmperorsBirthday,
    #       JPTennou-tanjyou-bi, JPTennou-tanjyou-bi.
    #   All the holiday functions are listed in the data file "holidays.R"
    #   Additional holidays, which are not yet available there, can be added
    #   to this data base file.

    # FUNCTION:

    if(names) {
        nams <- names(Holiday)
        if(is.null(nams)) {
            nams <- if(is.character(Holiday))
                        Holiday
                    else {
                        ## wrk <- as.character(as.list(substitute(Holiday))[-1])
                        if(is.name(substitute(Holiday)))
                            if(is.function(Holiday))
                                ## A snag here is that if the user has assigned the holiday
                                ## to a variable and uses that variable in the call, the name
                                ## will be that of the variable:
                                ##
                                ##  > tmp1a <- Easter
                                ##  > holiday(2024:2025, Holiday = tmp1a, names = TRUE)
                                ##  GMT
                                ##        tmp1a        tmp1a
                                ## [2024-03-31] [2025-04-20]
                                ##
                                ## the 'else' clause below handles the analogous case when tmp1a
                                ## contains more than one holidays.
                                all.names(substitute(Holiday), functions = FALSE)
                            else { # here Holiday is a variable assigned to by the user
                                wrk <- all.names(Holiday, functions = FALSE)
                                if(identical(wrk, character(0))) {

                                    cat("unable to determine names; using H1, H2, ...;\n")
                                    cat("see ?holiday for alternative ways to specify argument Holiday\n")
                                    cat("that will allow the holiday() to deduce the names.\n")
                                    NULL
                                } else
                                    wrk
                            }
                        else {
                            wrk <- all.names(Holiday, functions = FALSE)
                            if(identical(wrk, character(0)))
                                all.names(substitute(Holiday), functions = FALSE)
                            else
                                wrk
                        }

                    }
        }
    }

    if(is.language(Holiday))
        Holiday <- eval(Holiday)


    if(is.function(Holiday)) {
        FUN <- match.fun(Holiday) # 2026-08-26 TODO: redundant? (is.function(Holiday) is TRUE here
        ans <- as.character(FUN(year))
        ## TODO: test the case when wrk is empty
        if(names)
            names(ans) <- rep(nams, length(ans)) # length(nams) should be 1
    } else {
        nHolidays <- length(Holiday)
        if(names && is.null(nams))
            nams <- paste0("h", seq_len(nHolidays))

        ans <- character(0)
        for (i in seq_len(nHolidays)) {
            FUN <- match.fun(Holiday[[i]])
            wrk <- as.character(FUN(year))
            ## TODO: test the case when wrk is empty
            if(names)
                names(wrk) <- rep(nams[[i]], length(wrk))
            ans <- c(ans, wrk)
        }
    }

    # Classify as simple integer ISO date format CCYYMMDD
    ans <- timeDate(ans)

    # Return Value:
    ans
    }
})


################################################################################

## rest is added by GNB in August 2026

class(holiday) <- "holiday"

.na_holiday <- function(year = getRmetricsOptions("currentYear"), shift = 0) {
    ## timeDate(paste0(year, sprintf("%02d", month), sprintf("%02d", day)))
    timeDate(rep(NA_character_, length(year)))
}



`$.holiday` <- function(x, name) {
    obj <- get(name, envir = environment(x))
    if(is.function(obj))
        obj
    else if(is.na(obj)) {
        .na_holiday
    } else if(is.numeric(obj)) {
        if(obj < 101) { # 
            f <- get("Easter_shift", envir = environment(x))
            environment(f)$shift <- obj
            f
        } else {
            f <- get("holiday_from_date", envir = environment(x))
            environment(f)$.md <- obj
            f
        }
    } else
        stop("unknown holiday expression")
}

oHoliday <- environment(holiday)$Orthodox
jHoliday <- environment(holiday)$Orthodox$Julian
## todo: the symbols Orthodox and Julian from the above environments?
