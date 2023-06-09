/** 
 * @file  randtoolbox.c
 * @brief C file for all RNGs
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2009, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2009 Christophe Dutang, Petr Savicky. 
 * All rights reserved.
 *
 *      Redistribution and use in source and binary forms, with or without
 *      modification, are permitted provided that the following conditions are
 *      met:
 *      
 *          - Redistributions of source code must retain the above copyright
 *          notice, this list of conditions and the following disclaimer.
 *          - Redistributions in binary form must reproduce the above
 *          copyright notice, this list of conditions and the following
 *          disclaimer in the documentation and/or other materials provided
 *          with the distribution.
 *          - Neither the name of the Academy of Sciences of the Czech Republic
 *          nor the names of its contributors may be used to endorse or promote 
 *          products derived from this software without specific prior written
 *          permission.
 *     
 *      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *      OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *      SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *      THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *  
 */
/*
 *  WELL generators
 *  
 *			header file
 *
 */

//R header files
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/Error.h>

#include "config.h"
#include "locale.h"

//WELL
#include "WELL512a.h"
#include "WELL521a.h"
#include "WELL521b.h"
#include "WELL607a.h"
#include "WELL607b.h"
#include "WELL1024a.h"
#include "WELL1024b.h"

#include "WELL800a.h"
#include "WELL800b.h"
#include "WELL19937a.h"
#include "WELL19937b.h"
#include "WELL19937c.h"
#include "WELL21701a.h"
#include "WELL23209a.h"
#include "WELL23209b.h"
#include "WELL44497a.h"
#include "WELL44497b.h"

//time header files
#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_WINDOWS_H
# include <windows.h>
#endif

#if defined(HAVE_SSE2)
# include <emmintrin.h>
#endif


/* Functions accessed from .Call() */
SEXP doSetSeed4WELL(SEXP s);
SEXP doWELL(SEXP n, SEXP d, SEXP order, SEXP tempering, SEXP version);

/* utility functions */
void WELLrng(double *u, int nb, int dim, int order, int temper, int version);

void setSeed4WELL(long s);
void randSeedByArray(int length);
void randSeed(void);

void WELL_get_set_entry_point(void * p_user_unif_set_generator);
void initMT2002(unsigned int *seed, int *n, unsigned int *state);
extern void putRngWELL(int *porder, int *pversion, unsigned int *state);
extern void getRngWELL(int *porder, int *pversion, unsigned int *state);

