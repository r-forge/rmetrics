
library(randtoolbox)

#line 750 acebayes.R
method <- "quadrature"
#if(missing(nrq)) {
  nrq <- switch(method,
                quadrature = c(2, 8),
                NULL)
#}#  
nr <- nrq[[1]]
nq <- nrq[[2]]

allvars <- all.vars(~ x1 +x2 + x3 + x4)
paravars <- setdiff(allvars, ".")
p <- length(paravars)

#line 2322
Nq <- nq ; Nr <- nr

Q <- array(dim=c(p, p, Nq))

#
halton(p * p * Nq, dim = 1, normal = TRUE) #is infinite

big.ran.mat <- matrix(halton(p * p * Nq, dim = 1, normal = T), ncol = p)
big.ran.mat <- matrix(halton(p * p * Nq, dim = 1, normal = T, start=1), ncol = p)
for (i in 1:Nq) {
  ran.mat <- big.ran.mat[(1 + (i - 1) * p):(p + (i - 1) * p), ]
  qr <- qr(ran.mat)
  Q[,,i]<-qr.Q(qr)
}

Q2 <- array(dim=c(p, p, Nq))

big.ran.mat <- halton(p * Nq, dim = p, normal = T, start=1)
for (i in 1:Nq) {
  ran.mat <- big.ran.mat[(1 + (i - 1) * p):(p + (i - 1) * p), ]
  qr <- qr(ran.mat)
  Q2[,,i]<-qr.Q(qr)
}
