
df <- rulesFinCenter(listFinCenter()[1])
names(df)
colnames(df)
rownames(df)
colnames(df)[1] <- "LocalTime" # the name of the first column is the city.
                                        # we make it common for all fin centers

for( fc in listFinCenter()[-1]){
    wrk <- rulesFinCenter(fc)
    colnames(wrk)[1] <- "LocalTime"
    df <- rbind(df, wrk)
}

dim(df)


udf <- unique(df)
tmp_ind <- order(udf[ , "LocalTime"])
udf_sorted <- udf[tmp_ind, ]         

dim(udf_sorted)

dim(udf_sort())
dim(df)



dim(udf_sorted)

indx_moscow <- get_fc_index("Moscow", udf_sorted)
new_moscow <- fc_index_to_df(indx_moscow, udf_sorted, "Moscow")

waldo::compare(rulesFinCenter("Moscow"), new_moscow)

dput(indx_moscow)



dput_fc("Abidjan", udf_sorted)
dput_fc("Moscow", udf_sorted)

create_dst_file(c("Abidjan", "Moscow"))



## source("gnb_compact_dst_rules.R")
##  then run the commands below to compare with the existing rules

create_dst_file() # creates DaylightSavingTime.R and all_tz_rows.rda     
tzdb_cache <- new.env()                      # cache
all_tz_rows <- readRDS("all_tz_rows.rda")    # read the unique rows
ls()

## this creates the symbols, Abidjan, etc. in the current environment
source("DaylightSavingTime.R")


cities <- ls()[grepl("^[A-Z]", ls())]
## we use the symbols from the current env. created above to check that calling them
## equivalent to the current financial rules
all(
sapply(cities, function(fc){identical(match.fun(fc)(), get(fc, asNamespace("timeDate"))())})
)

##
