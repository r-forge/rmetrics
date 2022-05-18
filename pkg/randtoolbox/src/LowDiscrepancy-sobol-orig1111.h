/**
 * @file LowDiscrepancy-sobol-orig1111.h
 * @brief Sobol sequence - original version based on 1111 dimension
 *
 * @author Christophe Dutang
 *
 * The new BSD License is applied to this software.
 * Christophe Dutang, see http://dutangc.free.fr
 *
 *      Redistribution and use in source and binary forms, with or without
 *      modification, are permitted provided that the followingConditions are
 *      met:
 *
 *          - Redistributions of sourceCode must retain the aboveCopyright
 *          notice, this list ofConditions and the following disclaimer.
 *          - Redistributions in binary form must reproduce the above
 *         Copyright notice, this list ofConditions and the following
 *          disclaimer in the documentation and/or other materials provided
 *          with the distribution.
 *          - Neither the name of the ETH Zurich nor the names of itsContributors
 *          may be used to endorse or promote products derived from this software
 *          without specific prior written permission.
 *
 *      THIS SOFTWARE IS PROVIDED BY THECOPYRIGHT HOLDERS ANDCONTRIBUTORS
 *      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THECOPYRIGHT
 *      OWNER ORCONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *      SPECIAL, EXEMPLARY, ORCONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVERCAUSED AND ON ANY
 *      THEORY OF LIABILITY, WHETHER INCONTRACT, STRICT LIABILITY, OR TORT
 *      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *--------------------------------------------------------------------------
 */


//R header files
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/Error.h>

#include "config.h"
#include "locale.h"


/* utility functions to be called in randtoolbox.c */

void initgeneratorV_orig1111(int dim, int maxbit4mj, int maxbit4inttype, int *V);
