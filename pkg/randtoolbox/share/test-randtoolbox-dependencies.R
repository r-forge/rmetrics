#library(RWsearch)

#crandb_down(dir="~")
#s_crandb("randtool", select="P")
#

#################################################################
# package that reverse depends on randtoolbox

library(devtools)


odir <- "~/Documents/recherche-enseignement/code/R/rmetrics/test-check-depend-pkg/"

revdependlist <- c("iterLap", "npsf", "TruncatedNormal")
revdependversion <- c("1.1-3", "0.8.0", "2.2.2")

install.packages(revdependlist, dependencies = TRUE)

rescheck <- list()

for(i in 1:length(revdependlist))
{
  pkg <- revdependlist[i]
  v <- revdependversion[i]
  
  urlpkg <- paste0("https://cran.r-project.org/src/contrib/", pkg, "_", v, ".tar.gz")
  destfilepkg <- paste0(odir, "/", pkg, "_", v, ".tar.gz")
  
  cat(paste0(pkg, "_", v, ".tar.gz"), "\n")
  
  download.file(urlpkg, destfile=destfilepkg)
  
  rescheck[[i]] <- check_built(path = destfilepkg, quiet=TRUE)
  
}

capture.output(rescheck, file=paste0(odir, "/", "revdependlist.txt"))


#################################################################
# package that reverse imports randtoolbox

odir <- "~/Documents/recherche-enseignement/code/R/rmetrics/test-check-import-pkg/"

revimportlist <- c("acebayes", "apollo", "BLPestimatoR", "calibrateBinary", "cepp", 
                   "copBasic", "DiceOptim",  "GPareto", "GPM", "joineRML", 
                   "KrigInv", "LSDsensitivity", "LVGP", "MBHdesign",  
                   "merlin", "minimaxdesign", "MRFA", "optim.functions", "pGPx", 
                   "qualpalr", "sensobol")
revimportversion <- c("1.10", "0.2.7", "0.3.2", "0.1", "1.7", 
                      "2.1.7", "2.1.1", "1.1.6", "3.0.1", "0.4.5", 
                      "1.4.1", "1.2.1", "2.1.5", "2.2.2", 
                      "0.1.0", "0.1.5", "0.4", "0.1", "0.1.1", 
                      "0.4.3", "1.1.1")

install.packages(revimportlist, dependencies = TRUE)

rescheck <- list()
for(i in 1:length(revimportlist))
{
  pkg <- revimportlist[i]
  v <- revimportversion[i]
  
  urlpkg <- paste0("https://cran.r-project.org/src/contrib/", pkg, "_", v, ".tar.gz")
  destfilepkg <- paste0(odir, "/", pkg, "_", v, ".tar.gz")
  
  cat(paste0(pkg, "_", v, ".tar.gz"), "\n")
  
  download.file(urlpkg, destfile=destfilepkg)
  
  rescheck[[i]] <- check_built(path = destfilepkg, quiet=TRUE)
  
}
capture.output(rescheck, file=paste0(odir, "/", "revimportlist.txt"))



#################################################################
# package that reverse suggest randtoolbox

revsuggestlist <- c("copula", "DiceDesign", "sensitivity")
revsuggestversion <- c("1.0-1", "1.9", "1.27.0")
odir <- "~/Documents/recherche-enseignement/code/R/rmetrics/test-check-suggest-pkg/"

install.packages(revsuggestlist, dependencies = TRUE)

rescheck <- list()
for(i in 1:length(revsuggestlist))
{
  pkg <- revsuggestlist[i]
  v <- revsuggestversion[i]
  
  urlpkg <- paste0("https://cran.r-project.org/src/contrib/", pkg, "_", v, ".tar.gz")
  destfilepkg <- paste0(odir, "/", pkg, "_", v, ".tar.gz")
  
  cat(paste0(pkg, "_", v, ".tar.gz"), "\n")
  
  download.file(urlpkg, destfile=destfilepkg)
  
  rescheck[[i]] <- check_built(path = destfilepkg, quiet=TRUE)
  
}
capture.output(rescheck, file=paste0(odir, "/", "revsuggestlist.txt"))
