
## only character from, to; by as for as.Date()
.gnb_timeSequence <- function(from, to, by = 1, format = NULL, zone = "New_York", FinCenter = "New_York") {
    dat <- seq(as.Date(from), as.Date(to), by = by)
    as.timeDate(dat, zone = zone, FinCenter = FinCenter)
}

as.timeDate(seq(as.Date("2022-06-12"), as.Date("2022-12-31"), by = 1), zone = "New_York", FinCenter="New_York")


NYSEspecials <- specialCleaned
NYSEspecials[ , 1] <- gsub("Sept[.]", "Sep.", NYSEspecials[ , 1])
for(i in 1:nrow(NYSEspecials)) {
    x <- NYSEspecials[i, 1]
    wrk <- try(timeDate(x, format = "%b. %d, %Y"))
    if(is.na(wrk))
        wrk <- try(timeDate(x, format = "%B %d, %Y"))
    if(!inherits(wrk, "try-error")  && !is.na(wrk))
        NYSEspecials[i, 1] <- as.character(wrk)
}


## several ranges are closed on Saturdays:
closed_Saturdays <-                     .gnb_timeSequence("1945-07-07", "1945-09-01")
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1946-06-01", "1946-09-28"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1947-05-31", "1947-09-27"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1948-05-29", "1948-09-25"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1949-05-28", "1949-09-24"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1950-06-03", "1950-09-30"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1951-06-02", "1951-09-29"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1952-05-31", "1952-09-27"))
closed_Saturdays[dayOfWeek(closed_Saturdays) == "Sat"]
closed_Saturdays <- closed_Saturdays[dayOfWeek(closed_Saturdays) == "Sat"]
closed_Saturdays



nyseA <- read.delim2("NYSEspecials.txt", sep = "|", header = TRUE, strip.white=TRUE)
nyseB <- nyseA
for(i in 1:nrow(nyseB)) {
    x <- nyseB[i, 1]
    wrk <- try(timeDate(x, format = "%b. %d, %Y"))
    if(is.na(wrk))
        wrk <- try(timeDate(x, format = "%B %d, %Y"))
    if(!inherits(wrk, "try-error")  && !is.na(wrk))
        nyseB[i, 1] <- as.character(wrk)
}

nyseB[ , 1:2]


nyseB[90,3]
## [1] "Closed pending outbreak of World War I. Reopened for trading in bonds with price restrictions on November 28, 1914; for trading in a limited number of stocks under price restrictions on December 12, 1914; and for trading in all stocks, under price restrictions, on December 15, 1914. All restrictions were removed April 1, 1915."
outbreak_WW1_1_closed <- .gnb_timeSequence("1914-07-31", "1914-11-27")
outbreak_WW1_2_reopened_trading_in_bonds <- .gnb_timeSequence("1914-11-28", "1914-12-11")
outbreak_WW1_3_also_trading_in_some_stocks_with_restr <- .gnb_timeSequence("1914-12-12", "1914-12-14")
outbreak_WW1_4_also_trading_in_all_stocks_with_restr <- .gnb_timeSequence("1914-12-15", "1915-03-31")




nyseB[268:278,2:3]
[1] "2:00 pm closing (nine days) due to back office work load."
## TODO: remove 272:273 (Sat-Sun)


nyseB[279,3]
## [1] "2:00 pm closing due to back office work load. 3:30 close resumed March 4."
early_closing_01_1968_2pm <- .gnb_timeSequence("1968-01-22", "1968-03-01")


nyseB[284,]
##                      date   dayWeek  expl
## 284 June 12-Dec. 31, 1968 (Wed-Tue)  Four day week (closed on Wednesdays or regular holiday) – Paperwork Crisis.
##
## TODO!


nyseB[286, ]
##                    date   dayWeek   expl
## 286 Jan. 2-July 3, 1969 (Thu-Thu)   Five day week resumed, but with curtailed hours: 2:00 pm closing – Paperwork Crisis.
early_closing_02_1969_2pm <- .gnb_timeSequence("1969-01-02", "1969-07-03")


nyseB[292, ]
##                     date   dayWeek                                expl
## 292 July 7-Sep. 26, 1969 (Mon-Fri) 2:30 pm closing – Paperwork Crisis.
early_closing_03_1969_2.30pm <- .gnb_timeSequence("1969-07-07", "1969-09-26")



nyseB[295, ] # remove this line and replace with the below
##
##           date   dayWeek  expl
## 295 1969-09-29 (Mon-Fri)  3:00 pm closing – Paperwork Crisis. Normal hours (10:00 am-3:30 pm) resumed May 4, 1970.
early_closing_04_1969_3pm <- .gnb_timeSequence("1969-09-29", "1970-05-01")


nyseB[397, ] 
##           date dayWeek  expl
## 397 2001-09-17  (Mon.)  Trading reopened Monday, September 17, 2001, at 9:33 am following two minutes of silence in honor of the victims of the attack on the World Trade Center and then the singing of God Bless America.
nyseB[397, 2] <- "(Mon)" # was: "(Mon.)"


write.table(nyseC, file = "nyseC.txt", sep = "|", quote = FALSE, row.names = FALSE)


nyseD <- read.delim2("nyseC.txt", sep = "|", header = TRUE, strip.white=TRUE)

colnames(nyseD)
## [1] "type"    "date"    "dayWeek" "expl"   

data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 60))

data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))[nyseD$type == "closed" , ]



> nyseD[c(220, 228:230, 232, 234, 237, 240), ]
        type                 date   dayWeek              expl
220 separate  July 7-Sep. 1, 1945 (Sat-Sat) Closed Saturdays.
228   closed June 1-Sep. 28, 1946 (Sat-Sat) Closed Saturdays.
229   closed May 31-Sep. 27, 1947 (Sat-Sat) Closed Saturdays.
230   closed May 29-Sep. 25, 1948 (Sat-Sat) Closed Saturdays.
232   closed May 28-Sep. 24, 1949 (Sat-Sat) Closed Saturdays.
234   closed June 3-Sep. 30, 1950 (Sat-Sat) Closed Saturdays.
237   closed June 2-Sep. 29, 1951 (Sat-Sat) Closed Saturdays.
240   closed May 31-Sep. 27, 1952 (Sat-Sat) Closed Saturdays.
> nyseD[c(220, 228:230, 232, 234, 237, 240), "type"]
[1] "separate" "closed"   "closed"   "closed"   "closed"   "closed"   "closed"   "closed"  
> nyseD[c(220, 228:230, 232, 234, 237, 240), "type"] <- "separate"


data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))

data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))[nyseD$type == "separate", ]

data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))[grepl("^early", nyseD$type), ]

data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))[grepl("^closed", nyseD$type), ]

data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))[grepl("^open", nyseD$type), ]

################################################################################

## closings
data.frame(type = nyseD$type, date = nyseD$date, dayWeek = nyseD$dayWeek, expl = substr(nyseD$expl, 1, 70))[grepl("^closed", nyseD$type), ]$date

specialClosings <- nyseD[grepl("^closed", nyseD$type), ]$date
earlyClosings <- nyseD[grepl("^early", nyseD$type), ]$date

which(as.character(holidayNYSE(1885:2022) ) %in% specialClosings)
## [1] 305 783
as.character(holidayNYSE(1885:2022) )[c(305, 783)]
## [1] "1918-11-11" "1965-12-24"

closingsA <- timeDate(specialClosings, zone = "New_York", FinCenter = "New_York")
earlyA <- timeDate(earlyClosings, zone = "New_York", FinCenter = "New_York")

## [1] "Closed pending outbreak of World War I. Reopened for trading in bonds with price restrictions on November 28, 1914; for trading in a limited number of stocks under price restrictions on December 12, 1914; and for trading in all stocks, under price restrictions, on December 15, 1914. All restrictions were removed April 1, 1915."

## closed
outbreak_WW1_1_closed <- .gnb_timeSequence("1914-07-31", "1914-11-27", zone = "New_York", FinCenter = "New_York")

## special info? partial?
outbreak_WW1_2_reopened_trading_in_bonds <- .gnb_timeSequence("1914-11-28", "1914-12-11", zone = "New_York", FinCenter = "New_York")
outbreak_WW1_3_also_trading_in_some_stocks_with_restr <- .gnb_timeSequence("1914-12-12", "1914-12-14", zone = "New_York", FinCenter = "New_York")
outbreak_WW1_4_also_trading_in_all_stocks_with_restr <- .gnb_timeSequence("1914-12-15", "1915-03-31", zone = "New_York", FinCenter = "New_York")


## several ranges are "closed Saturdays". I intepret these as "closed on Satrudays",
## though it is strange. Where Saturdays opened at that time?
##
## Don't add them to holidays for now but keep for info.
closed_Saturdays <-                     .gnb_timeSequence("1945-07-07", "1945-09-01", zone = "New_York", FinCenter = "New_York")
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1946-06-01", "1946-09-28", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1947-05-31", "1947-09-27", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1948-05-29", "1948-09-25", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1949-05-28", "1949-09-24", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1950-06-03", "1950-09-30", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1951-06-02", "1951-09-29", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays <- c(closed_Saturdays, .gnb_timeSequence("1952-05-31", "1952-09-27", zone = "New_York", FinCenter = "New_York"))
closed_Saturdays[dayOfWeek(closed_Saturdays) == "Sat"]
closed_Saturdays <- closed_Saturdays[dayOfWeek(closed_Saturdays) == "Sat"]
closed_Saturdays


##
## [1] "2:00 pm closing due to back office work load. 3:30 close resumed March 4."
early_closing_01_1968_2pm <- .gnb_timeSequence("1968-01-22", "1968-03-01", zone = "New_York", FinCenter = "New_York")


##                      date   dayWeek  expl
## 284 June 12-Dec. 31, 1968 (Wed-Tue)  Four day week (closed on Wednesdays or regular holiday) – Paperwork Crisis.
##
four_day_week_1968 <- .gnb_timeSequence("1968-06-12", "1968-12-31", zone = "New_York", FinCenter = "New_York")
## closed on Wed but not if a holiday in the week.
##
four_day_week_1968_Wed <- four_day_week_1968[dayOfWeek(four_day_week_1968) == "Wed"]

holidayNYSE(1968)[-(1:4)]
## NewYork
## [1] [1968-07-04] [1968-09-02] [1968-11-05] [1968-11-28] [1968-12-25]
four_day_week_1968_Wed
## 
##  [1] [1968-06-12] [1968-06-19] [1968-06-26] [1968-07-03] [1968-07-10] [1968-07-17] [1968-07-24] [1968-07-31] [1968-08-07] [1968-08-14]
## [11] [1968-08-21] [1968-08-28] [1968-09-04] [1968-09-11] [1968-09-18] [1968-09-25] [1968-10-02] [1968-10-09] [1968-10-16] [1968-10-23]
## [21] [1968-10-30] [1968-11-06] [1968-11-13] [1968-11-20] [1968-11-27] [1968-12-04] [1968-12-11] [1968-12-18] [1968-12-25]
##
## drop Wed which are holiday (here Christmas) or their week contains holiday(s)
##
## closed
closings_four_day_week_1968 <- four_day_week_1968_Wed[-c(4, 13, 22, 25, 29)]


##                    date   dayWeek   expl
## 286 Jan. 2-July 3, 1969 (Thu-Thu)   Five day week resumed, but with curtailed hours: 2:00 pm closing – Paperwork Crisis.
early_closing_02_1969_2pm <- .gnb_timeSequence("1969-01-02", "1969-07-03", zone = "New_York", FinCenter = "New_York")


##                     date   dayWeek                                expl
## 292 July 7-Sep. 26, 1969 (Mon-Fri) 2:30 pm closing – Paperwork Crisis.
early_closing_03_1969_2.30pm <- .gnb_timeSequence("1969-07-07", "1969-09-26", zone = "New_York", FinCenter = "New_York")



##
##           date   dayWeek  expl
## 295 1969-09-29 (Mon-Fri)  3:00 pm closing – Paperwork Crisis. Normal hours (10:00 am-3:30 pm) resumed May 4, 1970.
early_closing_04_1969_3pm <- .gnb_timeSequence("1969-09-29", "1970-05-01", zone = "New_York", FinCenter = "New_York")


################################################################################
#### assemble



wrk_specialClosingsNYSE <- sort(c(
    closingsA,
    outbreak_WW1_1_closed,
    ## closed_Saturdays, # see remarks where it is defined
    closings_four_day_week_1968,
    timeDate(c("2012-10-29", "2012-10-30"), zone = "New_York", FinCenter = "New_York") # hurricane Sandy
    ))

wrk_earlyClosingsNYSE <- sort(c(
    earlyA,
    early_closing_01_1968_2pm,
    early_closing_02_1969_2pm,
    early_closing_03_1969_2.30pm,
    early_closing_04_1969_3pm
    ))

nyse_special_closings <- wrk_specialClosingsNYSE
nyse_early_closings <- wrk_earlyClosingsNYSE

## save as system data - sysdata.rda
save(nyse_special_closings, nyse_early_closings, file = "sysdata.rda", version = 2)



## verify that the dates from issue #1356 are in the set
sapply(c("1994-04-27", "2001-09-11", "2001-09-12", "2001-09-13", "2001-09-14", "2004-06-11", "2007-01-02"),
       function(x) any(timeDate(x, zone = "New_York", FinCenter = "New_York") ==  wrk_specialClosingsNYSE))
## 1994-04-27 2001-09-11 2001-09-12 2001-09-13 2001-09-14 2004-06-11 2007-01-02 
##       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE 


## Jonathan Owen, comment for #1356:
##   > The Christmas and New Years Day holidays for 2011 and 2012 are also incorrect (should be 12/26/2011 and 1/2/2012).
##
## These look ok to me. Not that NewYearDay() and ChristmasDay() return the dates of the
## feasts. These are converted by holidayXXXX() to the corresponding closing day for the
## exchange.
##  



