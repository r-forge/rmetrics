
setwd("~/Documents/recherche-enseignement/code/R/rmetrics/Rmetrics2/rmetrics/pkg/randtoolbox/share")

library(randtoolbox)

write.csv(sobol(100, 20), file="sobol.check.csv", row.names = FALSE)

write.csv(sobol(10000, 1111, method="Fortran", scrambling=0), file="sobol.scrambling0.dim1111.csv", row.names = FALSE)
write.csv(sobol(10000, 1111, method="Fortran", scrambling=1), file="sobol.scrambling1.dim1111.csv", row.names = FALSE)
write.csv(sobol(10000, 1111, method="Fortran", scrambling=2), file="sobol.scrambling2.dim1111.csv", row.names = FALSE)
write.csv(sobol(10000, 1111, method="Fortran", scrambling=3), file="sobol.scrambling3.dim1111.csv", row.names = FALSE)


library(randsobolfortran)

write.csv(sobol.fortran(10000, 1111, method="Fortran", scrambling=0), file="sobol.scrambling0.orig1111.csv", row.names = FALSE)
write.csv(sobol.fortran(10000, 1111, method="Fortran", scrambling=1), file="sobol.scrambling1.orig1111.csv", row.names = FALSE)
write.csv(sobol.fortran(10000, 1111, method="Fortran", scrambling=2), file="sobol.scrambling2.orig1111.csv", row.names = FALSE)
write.csv(sobol.fortran(10000, 1111, method="Fortran", scrambling=3), file="sobol.scrambling3.orig1111.csv", row.names = FALSE)


x<- read.csv("sobol.scrambling0.dim1111.csv", header=FALSE)
y<- read.csv("sobol.scrambling0.orig1111.csv", header=FALSE)
stopifnot(all(x == y))

x<- read.csv("sobol.scrambling1.dim1111.csv", header=FALSE)
y<- read.csv("sobol.scrambling1.orig1111.csv", header=FALSE)
stopifnot(all(x == y))

x<- read.csv("sobol.scrambling2.dim1111.csv", header=FALSE)
y<- read.csv("sobol.scrambling2.orig1111.csv", header=FALSE)
stopifnot(all(x == y))

x<- read.csv("sobol.scrambling3.dim1111.csv", header=FALSE)
y<- read.csv("sobol.scrambling3.orig1111.csv", header=FALSE)
stopifnot(all(x == y))

rm(x,y)
