/** 
 * @file  congruRand.h
 * @brief header file for congruential RNG
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 * Copyright (C) 2022, Christophe Dutang
 * # remove a warning: this old-style function definition is not preceded by a prototype
 * # raised by 
 * > clang -DNDEBUG   -isystem /usr/local/clang15/include                                      \
 * -I"/Library/Frameworks/R.framework/Headers"  -fpic  -O3 -Wall -pedantic -Wstrict-prototypes \
 * -c congruRand.c -o congruRand.o
 *
 * Copyright (C) 2009, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * Christophe Dutang, see http://dutangc.free.fr
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
/*****************************************************************************
 *  Congruential random number generators
 *    
 *      header file
 *
 */

//R header files
#include <R.h>
#include <Rmath.h>

#include "config.h"
#include "locale.h"


/* 
 * 64-bit int size type
 * similar to SFMT.h: see http://en.wikibooks.org/wiki/C_Programming/C_Reference/stdint.h 
 * and p150 of Write Portable Code by Brian Hook
 */
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
 #include <inttypes.h>
 #define HAVE_INT32_64_DEFINED 1
#elif defined(_MSC_VER) || defined(__BORLANDC__) || defined(__WATCOMC__)
 typedef unsigned int uint32_t;
 typedef unsigned __int64 uint64_t;
 #define inline __inline
 #define HAVE_INT32_64_DEFINED 1
#elif defined(__LP64__) || defined(__powerpc64__)
 typedef unsigned int uint32_t;
 typedef unsigned long uint64_t;
 #define inline __inline
 #define HAVE_INT32_64_DEFINED 1
#else
 #include <inttypes.h>
 #if defined(__GNUC__)
 #define inline __inline__
 #endif
 #define HAVE_INT32_64_DEFINED 1
#endif

/*64-bit int size specification for printf family*/
#ifndef PRIu64
 #if defined(_MSC_VER) || defined(__BORLANDC__)
  #define PRIu64 "I64u"
  #define PRIx64 "I64x"
 #else
  #define PRIu64 "llu"
  #define PRIx64 "llx"
 #endif
#endif

/*64-bit int size specification for scanf family*/
#ifndef SCNu64
 #if defined(_MSC_VER) || defined(__BORLANDC__)
  #define SCNu64 "I64u"
 #else
  #define SCNu64 "llu"
 #endif
#endif


#ifndef congruRand_H
#define congruRand_H

/* prototype defined in runifInterface.c */
void user_unif_set_generator(int gener, void (*selected_init)(unsigned int), double (*selected_rand)(void));
/* prototype defined in congruRand.c */
double user_unif_rand_congru(void);
void user_unif_init_congru(uint32_t seed);

double get_congruRand(void);
int check_congruRand(uint64_t mod, uint64_t mask, uint64_t mult, uint64_t incr, uint64_t seed);
void set_congruRand(uint64_t inp_mod, uint64_t inp_mult, uint64_t inp_incr, uint64_t inp_seed, uint64_t inp_mask);
void get_seed_congruRand(uint64_t *out_seed);

/* utility function to convert unsigned long long to string */
void ulltostr(uint64_t value, char* stroutput, int base);

/* Functions accessed from .C() */
void get_state_congru(char **params, char **seed);
void check_state_congru(char **params, char **seed, int *err);
void put_state_congru(char **params, char **seed, int *err);

#endif
