library(randtoolbox)

n <- 5

#### n argument ####
try(sobol(-1))

#### dim argument ####
try(sobol(1, 0))


#### init argument ####
sobol(n)
randtoolbox:::.getrandtoolboxEnv(".sobol.seed")
sobol(n, init=TRUE)
randtoolbox:::.getrandtoolboxEnv(".sobol.seed")
sobol(n, init=FALSE)
try(sobol(5, init="a"))

#### mixed argument ####

try(sobol(1, mixed=3))
sobol(n, mixed=TRUE)

#### normal argument ####
sobol(n, normal=TRUE)
try(sobol(3, normal=1))


#### start argument ####
try(sobol(3, start="3"))
sobol(n, start=0)
sobol(n, start=1)


