## 
# @file  quasiRNG.R
# @brief R file for all quasi RNGs
#
# @author Christophe Dutang
# @author Diethelm Wuertz 
#
# Copyright (C) 2009, Diethelm Wuertz, ETH Zurich. 
# Copyright (C) 2009-2021, Christophe Dutang, 
# Christophe Dutang, see http://dutangc.free.fr
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2019 Christophe Dutang, Diethelm Wuertz. 
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
#          - Neither the name of the ETH Zurich nor the names of its 
#          contributors may be used to endorse or promote 
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
### quasi random generation
###
###			R functions
### 




sobol.fortran <- function (n, dim = 1, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                   mixed = FALSE, method="Fortran", mexp = 19937, start = 1,
                   maxit = 10)
{   
  ## Check arguments
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >  1)
    stop("invalid argument 'dim'")
  if(dim < 1 || dim > 1111) #prepare the future release
    stop("invalid argument 'dim'")
  if(!is.logical(init))
    stop("invalid argument 'init'")
  if(!is.logical(mixed))
    stop("invalid argument 'mixed'")
  if(!is.logical(normal))
    stop("invalid argument 'normal'")
  if(!is.numeric(start))
    stop("invalid argument 'start'")
  if( !any(scrambling == 0:3) )
    stop("invalid argument 'scrambling'")   
  if(!is.numeric(mexp))
    stop("invalid argument 'mexp'")
  if(!is.numeric(maxit))
    stop("invalid argument 'maxit'")
  method <- match.arg(method, "Fortran")
  
  #for scrambled sequences when sobol_fortran() generates numbers outside [0,1)
  if(maxit <= 0 || maxit > 1e3)
    stop("maxit should be a positive integer below 1000.")
  
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argument 'n'")
  if(nb == 0) return(numeric(0))
  
  #not necessary
  #if(init && start != 0 && !normal) 
  #  warning("You should start your sequence from 0 as recommended by Owen (2020).")
  
  scramblmixed <- scrambling > 0 || mixed
  
  if(scrambling > 0 && mixed)
    warnings("only scrambling is used.")
  if(scrambling > 0)
  {
    if(is.null(seed))
      seed <- 4711 #default value
    else
      seed <- as.integer(seed) #convert it to integer
  }else if(scrambling == 0)
  {
    if(mixed)
    {
      seed <- as.integer(round(2^30*runif(1)))
      iter <- 0
      while(is.na(seed) && iter < maxit)
      {
        iter <- iter + 1
        seed <- as.integer(round(2^30*runif(1)))
      }
      if(iter == maxit)
        stop("100 calls to as.integer(round(2^30*runif(1))) have all generated NA, so we resign.") 
    }else
      seed <- 0 #default value for pure QMC
    
  }
  
  if(method == "Fortran")
  {  
    # Description:
    #   Uniform Sobol Low Discrepancy Sequence
    # Details:
    #   DIMENSION : dimension <= 1111
    #           N : LD numbers to create
    #  SCRAMBLING : One of the numbers 0,1,2,3
    
    
    # Restart Settings:
    if (init) 
      .setrandsobolfortranEnv(.sobol.seed = 
                           list(quasi = rep(0, dim), ll = 0, count = 0, sv = rep(0, dim*30), seed = seed))
    if(!exists(".sobol.seed", envir=.randsobolfortranEnv, mode="list"))
      stop("Sobol algorithm not initialized.")
    
    if(!init) #starting from last point
    {
      # Generate:
      qn <- numeric(nb * dim)
      
      #  SUBROUTINE SOBOL_F(QN, N, DIMEN, QUASI, LL, COUNT, SV, IFLAG, iSEED, INIT, TRANSFORM)
      #implemented in src/LowDiscrepancy.f
      result <- .Fortran(CF_sobol_f,
                         as.double( qn ),
                         as.integer( nb ),
                         as.integer( dim ),
                         as.double ( .getrandsobolfortranEnv(".sobol.seed")$quasi ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$ll ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$count ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$sv ),
                         as.integer( scrambling ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$seed ),
                         as.integer( init ),
                         as.integer( 0 ),
                         PACKAGE = "randsobolfortran")
      F_sobol <- TRUE
      # Deviates:
      sobolres <- matrix(result[[1]], ncol = dim)
    }else if(init && start == 0) #init = TRUE, use starting point 0
    {
      
      if(nb > 1)
      {
        # Generate nb-1 points
        qn <- numeric((nb-1) * dim)
        
        #  SUBROUTINE SOBOL_F(QN, N, DIMEN, QUASI, LL, COUNT, SV, IFLAG, iSEED, INIT, TRANSFORM)
        #implemented in src/LowDiscrepancy.f
        result <- .Fortran(CF_sobol_f,
                           as.double( qn ),
                           as.integer( nb-1 ),
                           as.integer( dim ),
                           as.double ( .getrandsobolfortranEnv(".sobol.seed")$quasi ),
                           as.integer( .getrandsobolfortranEnv(".sobol.seed")$ll ),
                           as.integer( .getrandsobolfortranEnv(".sobol.seed")$count ),
                           as.integer( .getrandsobolfortranEnv(".sobol.seed")$sv ),
                           as.integer( scrambling ),
                           as.integer( .getrandsobolfortranEnv(".sobol.seed")$seed ),
                           as.integer( init ),
                           as.integer( 0 ),
                           PACKAGE = "randsobolfortran")
        F_sobol <- TRUE
        # Deviates:
        sobolres <- matrix(result[[1]], ncol = dim)
        #add 0 for starting point
        if(dim == 1)
          sobolres <- c(0, sobolres)
        else
          sobolres <- rbind(0, sobolres)
      }else #sequence is only 0
      {
        F_sobol <- FALSE
        sobolres <- matrix(0, ncol=dim)
      }
    }else if(init && start == 1) #init = TRUE, use starting 1 
    {
      # Generate nb points
      qn <- numeric(nb * dim)
      
      #  SUBROUTINE SOBOL_F(QN, N, DIMEN, QUASI, LL, COUNT, SV, IFLAG, iSEED, INIT, TRANSFORM)
      #implemented in src/LowDiscrepancy.f
      result <- .Fortran(CF_sobol_f,
                         as.double( qn ),
                         as.integer( nb ),
                         as.integer( dim ),
                         as.double ( .getrandsobolfortranEnv(".sobol.seed")$quasi ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$ll ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$count ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$sv ),
                         as.integer( scrambling ),
                         as.integer( .getrandsobolfortranEnv(".sobol.seed")$seed ),
                         as.integer( init ),
                         as.integer( 0 ),
                         PACKAGE = "randsobolfortran")
      F_sobol <- TRUE
      # Deviates:
      sobolres <- matrix(result[[1]], ncol = dim)
    }else 
      stop("wrong start value.")
    
    #check value inside [0,1)
    if(scramblmixed)
    {  
      if(any(sobolres >= 1 | sobolres < 0))
      {
        warning("A call to sobol() generate numerics outside [0,1), so seed is randomized.")
        
        iter <- 0
        while(any(sobolres >= 1 | sobolres < 0) && iter < maxit)
        {
          iter <- iter + 1
          myrandseed <- (2^32-1)*runif(1)
          result <- .Fortran(CF_sobol_f,
                             as.double( qn ),
                             as.integer( nb ),
                             as.integer( dim ),
                             as.double ( .getrandsobolfortranEnv(".sobol.seed")$quasi ),
                             as.integer( .getrandsobolfortranEnv(".sobol.seed")$ll ),
                             as.integer( .getrandsobolfortranEnv(".sobol.seed")$count ),
                             as.integer( .getrandsobolfortranEnv(".sobol.seed")$sv ),
                             as.integer( scrambling ),
                             as.integer( myrandseed ),
                             as.integer( init ),
                             as.integer( 0 ),
                             PACKAGE = "randsobolfortran")
          sobolres <- matrix(result[[1]], ncol = dim)
        }
        if(iter == maxit)
          stop("100 calls to sobol() have all generated (some) numerics outside [0,1), so we resign.")
        #=> else appropriate seed found
        
      }
    }else
    {
      if(any(sobolres >= 1 | sobolres < 0))
        warning("A call to sobol() generate numerics outside [0,1).")
    }
    
    # For the next numbers save (only used if init=FALSE in the next call)
    if(F_sobol) 
      .setrandsobolfortranEnv(.sobol.seed = list(quasi = result[[4]], ll = result[[5]],
                                                count = result[[6]], sv = result[[7]], 
                                                seed = result[[9]]))
  }
  
  ## Normal transformation
  if(normal)
    sobolres <- qnorm(sobolres)   
  
  # Return Value:
  if(dim == 1)
    as.vector(sobolres)
  else
    as.matrix(sobolres)    
}



