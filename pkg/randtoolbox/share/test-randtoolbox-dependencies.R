#library(RWsearch)

#crandb_down(dir="~")
#s_crandb("randtool", select="P")
#

library(devtools)
library(RWsearch)
crandb_down()



#################################################################
# package that reverse depends on randtoolbox


odir <- "~/Documents/recherche-enseignement/code/R/rmetrics/test-check-depend-pkg/"

revdependlist <- p_deps("randtoolbox", which="D", reverse=TRUE)$randtoolbox
revdependversion <- p_vers(revdependlist)[,"version"]

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

revimportlist <- p_deps("randtoolbox", which="I", reverse=TRUE)$randtoolbox
revimportversion <- p_vers(revimportlist)[,"crandb"]

install.packages(revimportlist, dependencies = TRUE)

rescheck <- list()
for(i in 1:length(revimportlist))
{
  pkg <- revimportlist[i]
  v <- revimportversion[i]
  
  urlpkg <- paste0("https://cran.r-project.org/src/contrib/", pkg, "_", v, ".tar.gz")
  destfilepkg <- paste0(odir, "/", pkg, "_", v, ".tar.gz")
  
  cat(paste0(pkg, "_", v, ".tar.gz"), "\n")
  
  essai <- try(download.file(urlpkg, destfile=destfilepkg))
  if(!inherits(essai, "try-error"))
    rescheck[[i]] <- check_built(path = destfilepkg, quiet=TRUE)
  
}
capture.output(rescheck, file=paste0(odir, "/", "revimportlist.txt"))



#################################################################
# package that reverse suggest randtoolbox

revsuggestlist <- p_deps("randtoolbox", which="S", reverse=TRUE)$randtoolbox
revsuggestversion <- p_vers(revsuggestlist)[,"crandb"]

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
  
  essai <- try(download.file(urlpkg, destfile=destfilepkg))
  if(!inherits(essai, "try-error"))
    rescheck[[i]] <- check_built(path = destfilepkg, quiet=TRUE)
  
}
capture.output(rescheck, file=paste0(odir, "/", "revsuggestlist.txt"))
