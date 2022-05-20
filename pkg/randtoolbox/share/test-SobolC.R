library(randtoolbox)

n <- 2^3
d <- 3
for(start in 0:1)
{
  xonce <- sobol(n, d, start=start, method="C")
  xtwice <- sobol(n/2, d, start=start, method="C")
  randtoolbox:::.getrandtoolboxEnv(".sobol.seed")
  if(d == 1)
    xtwice <- c(xtwice, sobol(n/2, d, init=FALSE, method="C"))
  else 
    xtwice <- rbind(xtwice, sobol(n/2, d, init=FALSE, method="C"))
  cat("start", start, "equal", all(xonce == xtwice), "\n")
}

n <- 10000
d <- 1111
res <- sobol(n+1, dim=d, start=0, method="C")[-1, ]
head(res[,1:10])
     
#D Wuertz outputs
sobol.check.scrambling0.dim1111 <- read.csv("share/sobol.scrambling0.dim1111.csv", nrows = 10000)
head(sobol.check.scrambling0.dim1111[,1:10])



res[1:6, 1:10] == sobol.check.scrambling0.dim1111[1:6,1:10]

summary(apply(res[1:n, 1:d] == sobol.check.scrambling0.dim1111[1:n, 1:d], 2, mean))

all(res[1:n, 1:d] == sobol.check.scrambling0.dim1111[1:n, 1:d])


#F Kuo outputs
sobol.check.scrambling0.dim1111.fkuo <- read.table("share/sobol.orig1111.check.txt", nrow=n+1)[-1,]
dim(sobol.check.scrambling0.dim1111.fkuo)

apply(sobol.check.scrambling0.dim1111.fkuo[1:n, 1:d] == sobol.check.scrambling0.dim1111[1:n, 1:d], 2, mean)
all(sobol.check.scrambling0.dim1111.fkuo[1:n, 1:d] == sobol.check.scrambling0.dim1111[1:n, 1:d])

