/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

// This file is entirely based on the source files at
//   http://www.iro.umontreal.ca/~panneton/WELLRNG.html
// The parameters of the generator in this file are replaced by the parameters
// from Tables I and II in the original paper by F. Panneton, P. L'Ecuyer and M. Matsumoto.
// The function GetWELLRNG521b() was add for interface to R package randtoolbox/rngWELL
// by Ch. Dutang and P. Savicky.

/* the assignment of cases is as follows
 * state_i      function
 *
 *  0           case1
 *  1           case2
 *  2           case6
 *  ...         ...
 *  R-M1-1      case6
 *  R-M1        case5
 *  ...         ...
 *  R-M2-1      case5
 *  R-M2        case4
 *  ...         ...
 *  R-M3-1      case4
 *  R-M3        case3
 *  ...         ...
 *  R-1         case3
 */

#define W 32
#define R 17
#define P 23
#define MASKU (0xffffffffU>>(W-P))
#define MASKL (~MASKU)

#define M1 11
#define M2 10
#define M3 7

#include "WELLmatrices.h"
#include "WELLindexing.h"

#define FACT 2.32830643653869628906e-10

//array with the last R v_i,j's
static unsigned int STATE[R];
static unsigned int z0,z1,z2;
static int state_i=0;

static double case_1(void);
static double case_2(void);
static double case_3(void);
static double case_4(void);
static double case_5(void);
static double case_6(void);

double (*WELLRNG521b)(void);

void InitWELLRNG521b(unsigned int *init )
{
  int j;
  state_i=0;
  WELLRNG521b = case_1;
  for(j=0;j<R;j++)
    STATE[j]=init[j];
}

void GetWELLRNG521b (unsigned int *state)
{
  int j, k;
  j = 0;
  for (k = state_i; k < R; k++)
    state[j++] = STATE[k];
  for (k = 0; k < state_i; k++)
    state[j++] = STATE[k];
}

// state_i == 0
double case_1(void)
{
  z0     = (Vrm1Under & MASKL) | (Vrm2Under & MASKU);
  z1     = MAT0NEG(-21,V0) ^ MAT0POS(6,VM1);
  z2     = MAT7(VM2) ^ MAT0NEG(-13,VM3);
  newV1  = z1 ^ z2;
  newV0Under = MAT0POS(13,z0) ^ MAT3NEG(-10,z1) ^ MAT3NEG(-5,z2) ^ MAT0POS(13,newV1);
  state_i = R-1;
  WELLRNG521b = case_3;

  return ((double) STATE[state_i] * FACT);
}

// state_i == 1
static double case_2(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
  z1    = MAT0NEG(-21,V0) ^ MAT0POS(6,VM1);
  z2    = MAT7(VM2) ^ MAT0NEG(-13,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(13,z0) ^ MAT3NEG(-10,z1) ^ MAT3NEG(-5,z2) ^ MAT0POS(13,newV1);
  state_i=0;
  WELLRNG521b = case_1;
 
  return ((double) STATE[state_i] * FACT);
}

// R-1 >= state_i >= R-M3
static double case_3(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT0NEG(-21,V0) ^ MAT0POS(6,VM1Over);
  z2    = MAT7(VM2Over) ^ MAT0NEG(-13,VM3Over);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(13,z0) ^ MAT3NEG(-10,z1) ^ MAT3NEG(-5,z2) ^ MAT0POS(13,newV1);
  state_i--;
  if(state_i+M3<R)
    WELLRNG521b = case_4;
 
  return ((double) STATE[state_i] * FACT);
}

// R-M3-1 >= state_i >= R-M2
static double case_4(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT0NEG(-21,V0) ^ MAT0POS(6,VM1Over);
  z2    = MAT7(VM2Over) ^ MAT0NEG(-13,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(13,z0) ^ MAT3NEG(-10,z1) ^ MAT3NEG(-5,z2) ^ MAT0POS(13,newV1);
  state_i--;
  if (state_i+M2< R)
    WELLRNG521b = case_5;

  return ((double) STATE[state_i] * FACT);
}

// R-M2-1 >= state_i >= R-M1
static double case_5(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT0NEG(-21,V0) ^ MAT0POS(6,VM1Over);
  z2    = MAT7(VM2) ^ MAT0NEG(-13,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(13,z0) ^ MAT3NEG(-10,z1) ^ MAT3NEG(-5,z2) ^ MAT0POS(13,newV1);
  state_i--;
  if(state_i+M1 < R)
    WELLRNG521b = case_6;

  return ((double) STATE[state_i] * FACT);
}

// R-M1-1 >= state_i >= 2
static double case_6(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT0NEG(-21,V0) ^ MAT0POS(6,VM1);
  z2    = MAT7(VM2) ^ MAT0NEG(-13,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(13,z0) ^ MAT3NEG(-10,z1) ^ MAT3NEG(-5,z2) ^ MAT0POS(13,newV1);
  state_i--;
  if(state_i == 1 )
    WELLRNG521b = case_2;

  return ((double) STATE[state_i] * FACT);
}

