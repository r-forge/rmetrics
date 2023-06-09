## 
# @file  pseudoRNG.R
# @brief R file for all pseudo RNGs
#
# @author Christophe Dutang
# @author Petr Savicky 
#
#
# Copyright (C) 2019, Christophe Dutang, 
# Petr Savicky, Academy of Sciences of the Czech Republic. 
# Christophe Dutang, see http://dutangc.free.fr
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2019 Christophe Dutang, Petr Savicky. 
# All rights reserved.
#
#      Redistribution and use in source and binary forms, with or without
#      modification, are permitted provided that the following conditions are
#      met:
#      
#          - Redistributions of source code must retain the above copyright
#          notice, this list of conditions and the following disclaimer.
#          - Redistributions in binary form must reproduce the above
#          copyright notice, this list of conditions and the following
#          disclaimer in the documentation and/or other materials provided
#          with the distribution.
#          - Neither the name of the Academy of Sciences of the Czech Republic
#          nor the names of its contributors may be used to endorse or promote 
#          products derived from this software without specific prior written
#          permission.
#     
#      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#      OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#      SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#      THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  
#
#############################################################################
### pseudo random generation
###
###			R functions
### 


### set the seed ###

setSeed <- function(seed)
	invisible( .Call("doSetSeed", seed) )


### pseudo random generation ###

congruRand <- function(n, dim = 1, mod = 2^31-1, mult = 16807, incr = 0, echo)
{
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")
  if(!is.numeric(mod) || length(mod) !=1)
    stop("invalid argument 'mod'")
  if(!is.numeric(mult) || length(mult) != 1 || mult > mod || mult < 0)
    stop("invalid argument 'mult'")
  if(!is.numeric(incr) || length(incr) != 1 || incr > mod || incr < 0)
    stop("invalid argument 'incr'")    
  if(mod >= 2^64)
    warning("congruRand() does not correctly handle a modulo greater than or equal to 2^64, use runifInterface()")
  if(missing(echo))
    echo <- FALSE
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  #implemented in src/randtoolbox.c
  res <- .Call(CF_doCongruRand, nb, dim, mod, mult, incr, echo) 
  
  if(dim == 1)    
    as.vector(res)
  else
    as.matrix(res)
}

SFMT <- function(n, dim = 1, mexp = 19937, usepset = TRUE, withtorus = FALSE, usetime = FALSE)
{    
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")
  if(!is.logical(withtorus) && !is.numeric(withtorus))
    stop("invalid argument 'withtorus'")
  if(!is.numeric(mexp))
    stop("invalid argument 'mexp'")
  if(!is.logical(usepset))
    stop("invalid argument 'usepset'")
  
  authorizedParam <- c(607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091)
  
  if( !(mexp %in% authorizedParam) )
    stop("'mexp' must be in {607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091}. ")
  
  
  if(!is.logical(withtorus))
  {
    if(0 < withtorus && withtorus <= 1)
      nbTorus <- floor( withtorus * n )
    if(withtorus <=0 || withtorus > 1) 
      stop("invalid argument 'withtorus'")
  }
  if(is.logical(withtorus))
  {   
    if(!withtorus)
      nbTorus <- 0
    else
      stop("invalid argument 'withtorus'")
  }
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  if(nbTorus == 0)
  {
    #implemented in src/randtoolbox.c
    res <- .Call(CF_doSFMersenneTwister, nb, dim, mexp, usepset)
  }else
  {
    restorus <- torus(nbTorus, dim, mixed = FALSE, usetime = usetime)
    #implemented in src/randtoolbox.c
    res <- .Call(CF_doSFMersenneTwister, nb - nbTorus, dim, mexp, usepset)
    res <- rbind(res, as.matrix( restorus, nbTorus, dim) )
  }
  
  if(dim == 1)
    as.vector(res)
  else
    as.matrix(res)
}
 
WELL <- function(n, dim = 1, order = 512, temper = FALSE, version = "a")
{	
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")
  if(!is.numeric(order))
    stop("invalid argument 'order'")
  if( !(order %in% c(512, 521, 607, 800, 1024, 19937, 21701, 23209, 44497) ) )
    stop("'order' must be in {512, 521, 607, 800, 1024, 19937, 21071, 23209, 44497}.")
  if( !(version %in% c("a", "b") ) )
    stop("'version' must be either 'a' or 'b'.")
  
  if(!is.logical(temper))
    stop("invalid argument 'temper'")
  if(temper && order %in% c(512, 521, 607, 1024))
    stop("tempering impossible")
  
  zeversion <- 0
  if(version == "a")
    zeversion <- 1
  if(version == "b")
    zeversion <- 2
  if(zeversion == 0)
    stop("wrong version for WELL RNG")
  if(version == "b" && order %in% c(512,  21701) ) 
    stop("this WELL RNG does not have a 'b' version")
  
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  #implemented in src/randtoolbox.c
  res <- .Call(CF_doWELL, nb, dim, order, temper, zeversion)
  
  if(dim == 1)
    as.vector(res)
  else
    as.matrix(res)
}

knuthTAOCP <- function(n, dim = 1)
{
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")
  
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  #implemented in src/randtoolbox.c
  res <- .Call(CF_doKnuthTAOCP, nb, dim)
  
  if(dim == 1)
    as.vector(res)
  else
    as.matrix(res)
}

