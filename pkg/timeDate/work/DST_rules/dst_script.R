library(timeDate)

tz <- read.delim("zone1970.tab", comment.char = "#", header = FALSE)
colnames(tz) <- c("code", "coordinates", "TZ", "comments")
dim(tz)
colnames(tz)
## [1] "code"        "coordinates" "TZ"          "comments"   

fc_work <- tz[["TZ"]] ## tz[[3]]
fc_work <- sort(fc_work)
head(fc_work)
## [1] "Europe/Andorra"   "Asia/Dubai"       "Asia/Kabul"       "Europe/Tirane"   
## [5] "Asia/Yerevan"     "Antarctica/Casey"

fc_new <- fc_work[!(fc_work %in% listFinCenter())]  # rules not in timeDate yet
head(fc_new)

fc_rem <- listFinCenter()[!(listFinCenter() %in% fc_work)]  # rules not in fc_work
head(fc_rem)



any(is.na(listFinCenter()))


## backward
backward <- read.delim("backward", comment.char = "#", header = FALSE)


backward <- read.delim("backward", comment.char = "#", header = FALSE, sep = "",
                       col.names = c("Link", "Target", "Link_name", "Comment"))

nrow(backward[which(backward[["Link_name"]] %in% listFinCenter()), ])
                               
nrow(backward[which(!(backward[["Link_name"]] %in% listFinCenter())), ])

rownames(backward)

# names(backward)
[1] "Link"      "Target"    "Link_name" "Comment"  

## rows are in 'backward' data frame.
## by _manual_ comparison with the file!  !!! (take note!
pre1993_naming_convention <- c(1, 78)

# Two-part names that were renamed mostly to three-part names in 1995
                               
two_part_renamed_to 3_part_in 1995 <- c(79, 88)

# Pre-2013 practice, which typically had a Zone per zone.tab line
pre_2013_practice <- c(89, 195)

# Non-zone.tab locations with timestamps since 1970 that duplicate
# those of an existing location

non_zone.tab_locations <- c(196, 225)

# Alternate names for the same location
alternate_names_for_the_same_location <- c(226, 244) # 244 is last.

ls()

any(fc_new %in% backward[ ,3]) # FALSE (expected)
any(fc_new %in% backward[ ,2]) # TRUE (expected)

any(fc_rem %in% backward[ , 3]) # TRUE (expected)
any(fc_rem %in% backward[ , 2]) # FALSE (expected)

all(fc_rem %in% backward[ , 3]) # TRUE (expected)

fc_obsolete <- backward[ , 3]
fc_cur      <- backward[ , 2]
obs_table <- matrix(nrow = 0, ncol = 2)
for(fc in fc_rem) {
    ind <- which(fc_obsolete %in% fc)
    if(length(ind) == 0){
        cat(fc, " has no replacement in 'backward' database\n")
        next
    }
    if(length(ind) > 1)
        cat(fc, " appears more than once, taking the first one\n")
    cur <- fc_cur[ind[1]]
        # cat("\n    ", fc, "    ", cur, "\n")
    if(cur %in% fc_work)
        obs_table <- rbind(obs_table, c(fc, cur)) # to assign via "fc <- cur"
    else
        cat(" the suggested replacement, ", cur, ", for ", fc, " is not in 'fc_work'\n")
}

obs_table

## TODO: check nevertheless that the pairs are identical in the old database

## prepare assignments - need to take the after the last backslash.

stripped_obs_table <- apply(obs_table, 1:2,
                            function(fc){
                                fccity <- strsplit(fc, "/")[[1]]
                                City <- fccity[length(fccity)]
                            })
aliases <- paste0('"', stripped_obs_table[ , 1], '"', " <- ", stripped_obs_table[ , 2])
aliases <-c(
    "## this is for compatibility purpose with very old versions of timeDate;",
    "## but 'Frankfurt' is sensible anyway.",
    aliases
    )
cat("\n", aliases, sep = "\n")

library(timeDate)
## .genDaylightSavingTime(finCenter = fc_work)
.genDaylightSavingTime(finCenter = fc_work, aliases = aliases)
