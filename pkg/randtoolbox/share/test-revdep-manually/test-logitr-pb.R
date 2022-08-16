
library(logitr)

# Estimate a preference space model
mxl_pref <- logitr(
     data     = yogurt,
     outcome  = "choice",
     obsID    = "obsID",
     pars     = c("price", "feat", "brand"),
     randPars = c(feat = "n", brand = "n")
   )



getStandardDraws <- function(parIDs, numDraws) {
  numBetas <- length(parIDs$fixed) + length(parIDs$random)
  draws <- as.matrix(randtoolbox::halton(numDraws, numBetas, normal = TRUE))
  #draws[, parIDs$fixed] <- 0 * draws[, parIDs$fixed]
  return(draws)
}

getStandardDraws(list(fixed=c("price", "feat", "brand"), random=NULL),50)  
