require("stabledist")
pPareto <- stabledist:::pPareto

source(system.file("test-tools.R", package = "Matrix"))
                                        #-> identical3(), showProc.time(),...

options(pstable.debug = FALSE)
options(pstable.debug = TRUE)# want to see when uniroot() is called

stopifnot(all.equal(pstable(0.3, 0.75, -.5, tol= 1e-14),
		    0.6688726496, tol = 1e-8))
## was		    0.66887227658457, tol = 1e-10))
pstable(-4.5, alpha = 1, beta = 0.01)## gave integration error (now uniroot..)

## a "outer vectorized" version:
pstabALL <- function(x, alpha, beta, ...)
    sapply(alpha, function(alph)
           sapply(beta, function(bet)
			pstable(x, alph, bet, ...)))

alph.s <- (1:32)/16   # in (0, 2]
beta.s <- (-16:16)/16 # in [-1, 1]
stopifnot(pstabALL( Inf, alph.s, beta.s) == 1,
	  pstabALL(-Inf, alph.s, beta.s, log.p=TRUE) == -Inf,
	  pstabALL( 0,   alph.s, beta = 0) == 0.5,
	  TRUE)

##---- log-scale -------------
r <- curve(pstable(x, alpha=1.8, beta=.9,
                   lower.tail=FALSE, log.p=TRUE),
           5, 150, n=500,
           log="x",type="b", cex=.5)
curve(stabledist:::pPareto(x, alpha=1.8, beta=.9,
                           lower.tail=FALSE, log.p=TRUE), add=TRUE, col=2)
##--> clearly potential for improvement!

## the less extreme part - of that:
r <- curve(pstable(x, alpha=1.8, beta=.9,
                   lower.tail=FALSE, log.p=TRUE),
           1, 50, n=500, log="x")
curve(stabledist:::pPareto(x, alpha=1.8, beta=.9, lower.tail=FALSE, log.p=TRUE), add=TRUE, col=2)

## Check that   pstable() is the integral of dstable() --- using simple Simpson's rule

## in it's composite form:
## \int_a^b f(x) \, dx\approx \frac{h}{3}
##  \bigg[ f(x_0) + 2 \sum_{j=1}^{n/2-1}f(x_{2j}) +
##               + 4 \sum_{j=1}^{n/2}  f(x_{2j-1}) +
##        + f(x_n) \bigg],
intSimps <- function(fx, h) {
    stopifnot((n <- length(fx)) %% 2 == 0,
	      n >= 4, length(h) == 1, h > 0)
    n2 <- n %/% 2
    j2 <- 2L * seq_len(n2-1)
    j4 <- 2L * seq_len(n2) - 1L
    h/3 * sum(fx[1],  2* fx[j2], 4* fx[j4], fx[n])
}

chk.pd.stable <- function(alpha, beta, xmin=NA, xmax=NA,
                          n = 256, do.plot=TRUE,
                          comp.tol = 1e-13, eq.tol = 1e-3)
{
    stopifnot(n >= 20)
    if(is.na(xmin)) xmin <- qstable(0.01, alpha, beta)
    if(is.na(xmax)) xmax <- qstable(0.99, alpha, beta)
    dx <- ceiling(1024*grDevices::extendrange(r = c(xmin, xmax), f = 0.01))/1024
    h <- diff(dx)/n
    x <- seq(dx[1], dx[2], by = h)
    fx <- dstable(x, alpha=alpha, beta=beta, tol=  comp.tol)
    Fx <- pstable(x, alpha=alpha, beta=beta, tol=2*comp.tol)
    i.ev <- (i <- seq_along(x))[i %% 2 == 0 & i >= max(n/10, 16)]
    ## integrate from x[1] up to x[i]   (where i is even);
    ## the exact value will be F(x[i]) - F(x[1]) == Fx[i] - Fx[1]
    Fx. <- vapply(lapply(i.ev, seq_len),
                  function(ii) intSimps(fx[ii], h), 0)
    a.eq <- all.equal(Fx., Fx[i.ev] - Fx[1], tol = eq.tol)
    if(do.plot) {
        ## Show the fit
        plot(x, Fx - Fx[1], type = "l")
        lines(x[i.ev], Fx., col=adjustcolor("red", 0.5), lwd=3)
        op <- par(ask=TRUE) ; on.exit(par(op))
        ## show the "residual", i.e., the relative error
        plot(x[i.ev], 1- Fx./(Fx[i.ev] - Fx[1]),
             type = "l", xlim = range(x))
        abline(h=0, lty=3, lwd = .6)
    }

    if(!isTRUE(a.eq)) stop(a.eq)
    invisible(list(x=x, f=fx, F=Fx, i. = i.ev, F.appr. = Fx.))
}

pdf("pstab-ex.pdf")
op <- par(mfrow=2:1, mar = .1+c(3,3,1,1), mgp=c(1.5, 0.6,0))

c1 <- chk.pd.stable(.75, -.5,  -1, 1.5, eq.tol = .006)
c2 <- chk.pd.stable(.95, +0.6, -1, 1.5, eq.tol = .006)# with >= 50 warnings
## here are the "values"
with(c1, all.equal(F.appr., F[i.] - F[1], tol = 0)) # (.0041290 on 64-Lnx)
with(c2, all.equal(F.appr., F[i.] - F[1], tol = 0)) # (.0049307 on 64-Lnx)

showProc.time() #

c3 <- chk.pd.stable(.95, +0.9, -3, 15) # >= 50 warnings

curve(dstable(x, .999, -0.9), -20, 5, log="y")
curve(pstable(x, .999, -0.9), -20, 5, log="y")#-> using uniroot
c4 <- chk.pd.stable(.999, -0.9, -20, 5)

showProc.time() #

## alpha == 1 , small beta  ---- now perfect
curve(pstable(x, alpha=1, beta= .01), -6, 8, ylim=0:1)
abline(h=0:1, v=0, lty=3, col="gray30")
n <- length(x <- seq(-6,8, by = 1/16))
px <- pstable(x, alpha=1, beta= .01)
## now take approximation derivative by difference ratio:
x. <- (x[-n]+x[-1])/2
plot (x., diff(px)*16, type="l")
## now check convexity/concavity :
f2 <- diff(diff(px))
stopifnot(f2[x[-c(1,n)] < 0] > 0,
          f2[x[-c(1,n)] > 0] < 0)
## and compare with dstable() ... which actually shows dstable() problem:
fx. <- dstable(x., alpha=1, beta=.01)
lines(x., fx., col = 2, lwd=3, lty="5111")

if(dev.interactive()) {
    curve(dstable(x, 1.,    0.99),  -6, 50, log="y")# "uneven" (x < 0); 50 warnings
    curve(dstable(x, 1.001, 0.95), -10, 30, log="y")# much better
}
c5 <- chk.pd.stable(1.,    0.99,  -6, 50)# -> uniroot
c6 <- chk.pd.stable(1.001, 0.95, -10, 30)# -> uniroot; 2nd plot *clearly* shows problem
with(c5, all.equal(F.appr., F[i.] - F[1], tol = 0)) # .00058 on 64-Lnx
with(c6, all.equal(F.appr., F[i.] - F[1], tol = 0)) # 2.611e-5 on 64-Lnx

## right tail:
try(## FIXME:
c1.0 <- chk.pd.stable(1., 0.8,  -6, 500)# uniroot; rel.difference = .030
)
## show it more clearly
curve(pstable(x, alpha=1, beta=0.5), 20, 800, log="x", ylim=c(.97, 1))
curve(pPareto(x, alpha=1, beta=0.5), add=TRUE, col=2, lty=2)
abline(h=1, lty=3,col="gray")
# and similarly (alpha ~= 1, instead of == 1):
curve(pstable(x, alpha=1.001, beta=0.5), 20, 800, log="x", ylim=c(.97, 1))
curve(pPareto(x, alpha=1.001, beta=0.5), add=TRUE, col=2, lty=2)
abline(h=1, lty=3,col="gray")
## zoom
curve(pstable(x, alpha=1.001, beta=0.5), 100, 200, log="x")
curve(pPareto(x, alpha=1.001, beta=0.5), add=TRUE, col=2, lty=2)
## but  alpha = 1   is only somewhat better as approximation:
curve(pstable(x, alpha=1    , beta=0.5), add=TRUE, col=3,
      lwd=3, lty="5131")

showProc.time() #

c7 <- chk.pd.stable(1.2, -0.2,   -40, 30)
c8 <- chk.pd.stable(1.5, -0.999, -40, 30)# two warnings

showProc.time() #