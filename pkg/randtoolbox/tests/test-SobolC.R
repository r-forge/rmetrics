library(randtoolbox)

n <- 2^4

sobol(n, start=0, method="C")
res <- sobol(n, dim=10, start=0, method="C")[-1, ]
head(res)
     
sobol.check.scrambling0.dim1111 <- read.csv("share/sobol.scrambling0.dim1111.csv", nrows = 100)
head(sobol.check.scrambling0.dim1111[,1:10])



res[1:6, ] == head(sobol.check.scrambling0.dim1111[,1:10])


sobol.check.scrambling0.dim1111.fkuo <- read.table("share/sobol.orig1111.check.txt", nrow=11)[-1,]
sobol.check.scrambling0.dim1111.fkuo[, 1:10]


sobol.check.scrambling0.dim1111.fkuo[, 1:10] == sobol.check.scrambling0.dim1111[1:10, 1:10]
