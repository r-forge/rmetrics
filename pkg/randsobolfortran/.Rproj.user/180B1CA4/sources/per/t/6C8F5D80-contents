/** 
 * @file  init.c
 * @brief init file for all RNGs
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2017, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * Christophe Dutang, see http://dutangc.free.fr 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2013 Christophe Dutang, Petr Savicky. 
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
/*****************************************************************************
 *  randsobolfortran
 *
 *		init file
 *  
 *	Native routines registration, see 'writing R extensions'
 *
 */

#include <stdlib.h> //for NULL
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

//R header files
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/Error.h>

#include "config.h"
#include "locale.h"
#include "version.h"


//table of registration routines accessed with .C()
static const R_CMethodDef CEntries[] = {
  {"version_randsobolfortran",         (DL_FUNC) &version_randsobolfortran, 1}, //version.h
  {NULL, NULL, 0}
};


/* .Fortran calls defined LowDiscrepancy.f 
 */
/* DOES NOT WORK*/
extern void F77_NAME(sobol_f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

//table of registration routines accessed with .Fortran()
static const R_FortranMethodDef FortranEntries[] = {
  {"sobol_f", (DL_FUNC) &F77_NAME(sobol_f),  11}, //LowDiscrepancy.f
  {NULL, NULL, 0}
};


//there is no routine accessed with .External()

//table of all registered routines
void R_init_randsobolfortran(DllInfo *dll)
{
  //register method accessed with .C, .Call, .Fortran, .External respectively
  R_registerRoutines(dll, CEntries, NULL, FortranEntries, NULL); 
  
  /*dynamic lookup only for
  double *user_unif_rand(void);
  void user_unif_init(unsigned int seed);
  in src/runifInterface.h*/
  R_useDynamicSymbols(dll, TRUE);
  
}

