## take "extdata/msft.csv" from package timeSeries which is identical to the file
## "msft.dat.csv" "msft.dat.csv" which was formerly here in data/
fn <- system.file("extdata", "msft.csv", package = "timeSeries")

## We could read the first column as Date but do it as 'factor' for backward
## compatibility, since that was its class in object 'msft.dat'.
##
## but we do avoid mangling the name of the first column.
msft.dat <- utils::read.csv2(fn, colClasses = c("factor", rep("numeric", 5)),
                             dec = ".", check.names = FALSE)
rm(fn)
## a comparison of the old 'msft.dat' and the new one gave:
##
##     > all.equal(msft.dat, new_msft_dat)
##     [1] "Names: 1 string mismatch"
##
## as expected. The difference is in the name of the first column which was "X.Y..m..d"
## and now is "%Y-%m-%d".
