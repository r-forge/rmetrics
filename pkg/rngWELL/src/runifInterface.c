#include <R.h>
#include "WELL512a.h"
#include "WELL521a.h"
#include "WELL521b.h"
#include "WELL607a.h"
#include "WELL607b.h"
#include "WELL800a.h"
#include "WELL800b.h"
#include "WELL1024a.h"
#include "WELL1024b.h"
#include "WELL19937a.h"
#include "WELL19937c.h"
#include "WELL19937b.h"
#include "WELL21701a.h"
#include "WELL23209a.h"
#include "WELL23209b.h"
#include "WELL44497a.h"
#include "WELL44497b.h"

typedef void(*UserUnifSetGeneratorType)(int gener, void (*selected_init)(unsigned int), double (*selected_rand)());

#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];
int order, version;

UserUnifSetGeneratorType user_unif_set_generator;

void WELL_get_set_entry_point(UserUnifSetGeneratorType p_user_unif_set_generator)
{
	user_unif_set_generator = p_user_unif_set_generator;
}

void initMT2002(unsigned int *seed, int *n, unsigned int *state)
{
	int i;
	state[0] = *seed;
	for (i = 1; i < *n; i++)
	{
		state[i] = 1812433253UL * ( state[i - 1] ^ ( state[i - 1] >> 30 ) ) + i;
	}
}

void seedWELLRNG512a(unsigned int seed)
{
	int n=16;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG512a( seedArray );
}

void seedWELLRNG521a(unsigned int seed)
{
	int n=17;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG521a( seedArray );
}

void seedWELLRNG521b(unsigned int seed)
{
	int n=17;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG521b( seedArray );
}

void seedWELLRNG607a(unsigned int seed)
{
	int n=19;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG607a( seedArray );
}

void seedWELLRNG607b(unsigned int seed)
{
	int n=19;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG607b( seedArray );
}

void seedWELLRNG800a(unsigned int seed)
{
	int n=25;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG800a( seedArray );
}

void seedWELLRNG800b(unsigned int seed)
{
	int n=25;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG800b( seedArray );
}

void seedWELLRNG1024a(unsigned int seed)
{
	int n=32;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG1024a( seedArray );
}

void seedWELLRNG1024b(unsigned int seed)
{
	int n=32;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG1024b( seedArray );
}

void seedWELLRNG19937a(unsigned int seed)
{
	int n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937a( seedArray );
}

void seedWELLRNG19937c(unsigned int seed)
{
	int n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937c( seedArray );
}

void seedWELLRNG19937b(unsigned int seed)
{
	int n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937b( seedArray );
}

void seedWELLRNG21701a(unsigned int seed)
{
	int n=679;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG21701a( seedArray );
}

void seedWELLRNG23209a(unsigned int seed)
{
	int n=726;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG23209a( seedArray );
}

void seedWELLRNG23209b(unsigned int seed)
{
	int n=726;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG23209b( seedArray );
}

void seedWELLRNG44497a(unsigned int seed)
{
	int n=1391;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG44497a( seedArray );
}

void seedWELLRNG44497b(unsigned int seed)
{
	int n=1391;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG44497b( seedArray );
}

double generateWELLRNG512a()
{
	return WELLRNG512a();
}

double generateWELLRNG521a()
{
	return WELLRNG521a();
}

double generateWELLRNG521b()
{
	return WELLRNG521b();
}

double generateWELLRNG607a()
{
	return WELLRNG607a();
}

double generateWELLRNG607b()
{
	return WELLRNG607b();
}

double generateWELLRNG800a()
{
	return WELLRNG800a();
}

double generateWELLRNG800b()
{
	return WELLRNG800b();
}

double generateWELLRNG1024a()
{
	return WELLRNG1024a();
}

double generateWELLRNG1024b()
{
	return WELLRNG1024b();
}

double generateWELLRNG19937a()
{
	return WELLRNG19937a();
}

double generateWELLRNG19937c()
{
	return WELLRNG19937c();
}

double generateWELLRNG19937b()
{
	return WELLRNG19937b();
}

double generateWELLRNG21701a()
{
	return WELLRNG21701a();
}

double generateWELLRNG23209a()
{
	return WELLRNG23209a();
}

double generateWELLRNG23209b()
{
	return WELLRNG23209b();
}

double generateWELLRNG44497a()
{
	return WELLRNG44497a();
}

double generateWELLRNG44497b()
{
	return WELLRNG44497b();
}

// put state functions

void putRngWELL512(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG512a( state );
		user_unif_set_generator(2, seedWELLRNG512a, generateWELLRNG512a);
		break;
	}
}

void putRngWELL521(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG521a( state );
		user_unif_set_generator(2, seedWELLRNG521a, generateWELLRNG521a);
		break;
	case 2:
		InitWELLRNG521b( state );
		user_unif_set_generator(2, seedWELLRNG521b, generateWELLRNG521b);
		break;
	}
}

void putRngWELL607(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG607a( state );
		user_unif_set_generator(2, seedWELLRNG607a, generateWELLRNG607a);
		break;
	case 2:
		InitWELLRNG607b( state );
		user_unif_set_generator(2, seedWELLRNG607b, generateWELLRNG607b);
		break;
	}
}

void putRngWELL800(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG800a( state );
		user_unif_set_generator(2, seedWELLRNG800a, generateWELLRNG800a);
		break;
	case 2:
		InitWELLRNG800b( state );
		user_unif_set_generator(2, seedWELLRNG800b, generateWELLRNG800b);
		break;
	}
}

void putRngWELL1024(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG1024a( state );
		user_unif_set_generator(2, seedWELLRNG1024a, generateWELLRNG1024a);
		break;
	case 2:
		InitWELLRNG1024b( state );
		user_unif_set_generator(2, seedWELLRNG1024b, generateWELLRNG1024b);
		break;
	}
}

void putRngWELL19937(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG19937a( state );
		user_unif_set_generator(2, seedWELLRNG19937a, generateWELLRNG19937a);
		break;
	case 2:
		InitWELLRNG19937b( state );
		user_unif_set_generator(2, seedWELLRNG19937b, generateWELLRNG19937b);
		break;
	case 3:
		InitWELLRNG19937c( state );
		user_unif_set_generator(2, seedWELLRNG19937c, generateWELLRNG19937c);
		break;
	}
}

void putRngWELL21701(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG21701a( state );
		user_unif_set_generator(2, seedWELLRNG21701a, generateWELLRNG21701a);
		break;
	}
}

void putRngWELL23209(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG23209a( state );
		user_unif_set_generator(2, seedWELLRNG23209a, generateWELLRNG23209a);
		break;
	case 2:
		InitWELLRNG23209b( state );
		user_unif_set_generator(2, seedWELLRNG23209b, generateWELLRNG23209b);
		break;
	}
}

void putRngWELL44497(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG44497a( state );
		user_unif_set_generator(2, seedWELLRNG44497a, generateWELLRNG44497a);
		break;
	case 2:
		InitWELLRNG44497b( state );
		user_unif_set_generator(2, seedWELLRNG44497b, generateWELLRNG44497b);
		break;
	}
}

void putRngWELL(int *porder, int *pversion, unsigned int *state)
{
	order = *porder;
	version = *pversion;
	switch (order)
	{
		case 512: putRngWELL512(state); break;
		case 521: putRngWELL521(state); break;
		case 607: putRngWELL607(state); break;
		case 800: putRngWELL800(state); break;
		case 1024: putRngWELL1024(state); break;
		case 19937: putRngWELL19937(state); break;
		case 21701: putRngWELL21701(state); break;
		case 23209: putRngWELL23209(state); break;
		case 44497: putRngWELL44497(state); break;
	}
}

// get state functions

void getRngWELL512(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG512a( state );
	}
}

void getRngWELL521(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG521a( state );
		break;
	case 2:
		GetWELLRNG521b( state );
	}
}

void getRngWELL607(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG607a( state );
		break;
	case 2:
		GetWELLRNG607b( state );
	}
}

void getRngWELL800(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG800a( state );
		break;
	case 2:
		GetWELLRNG800b( state );
	}
}

void getRngWELL1024(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG1024a( state );
		break;
	case 2:
		GetWELLRNG1024b( state );
	}
}

void getRngWELL19937(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG19937a( state );
		break;
	case 2:
		GetWELLRNG19937b( state );
		break;
	case 3:
		GetWELLRNG19937c( state );
	}
}

void getRngWELL21701(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG21701a( state );
	}
}

void getRngWELL23209(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG23209a( state );
		break;
	case 2:
		GetWELLRNG23209b( state );
	}
}

void getRngWELL44497(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG44497a( state );
		break;
	case 2:
		GetWELLRNG44497b( state );
	}
}

void getRngWELL(int *porder, int *pversion, unsigned int *state)
{
	*porder = order;
	*pversion = version;
	switch (order)
	{
		case 512: getRngWELL512(state); break;
		case 521: getRngWELL521(state); break;
		case 607: getRngWELL607(state); break;
		case 800: getRngWELL800(state); break;
		case 1024: getRngWELL1024(state); break;
		case 19937: getRngWELL19937(state); break;
		case 21701: getRngWELL21701(state); break;
		case 23209: getRngWELL23209(state); break;
		case 44497: getRngWELL44497(state); break;
	}
}

