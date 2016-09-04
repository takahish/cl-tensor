%module gsl_rng

%{
#include <stdlib.h>
#include <gsl/gsl_types.h>
#include <gsl/gsl_inline.h>
#include <gsl/gsl_rng.h>
%}

typedef unsigned int size_t;

struct gsl_rng_type
{
  const char *name;
  unsigned long int max;
  unsigned long int min;
  size_t size;
  void (*set) (void *state, unsigned long int seed);
  unsigned long int (*get) (void *state);
  double (*get_double) (void *state);
};

struct gsl_rng
{
  const gsl_rng_type * type;
  void *state;
};

/* These structs also need to appear in default.c so you can select
   them via the environment variable GSL_RNG_TYPE */

const gsl_rng_type *gsl_rng_borosh13;
const gsl_rng_type *gsl_rng_coveyou;
const gsl_rng_type *gsl_rng_cmrg;
const gsl_rng_type *gsl_rng_fishman18;
const gsl_rng_type *gsl_rng_fishman20;
const gsl_rng_type *gsl_rng_fishman2x;
const gsl_rng_type *gsl_rng_gfsr4;
const gsl_rng_type *gsl_rng_knuthran;
const gsl_rng_type *gsl_rng_knuthran2;
const gsl_rng_type *gsl_rng_knuthran2002;
const gsl_rng_type *gsl_rng_lecuyer21;
const gsl_rng_type *gsl_rng_minstd;
const gsl_rng_type *gsl_rng_mrg;
const gsl_rng_type *gsl_rng_mt19937;
const gsl_rng_type *gsl_rng_mt19937_1999;
const gsl_rng_type *gsl_rng_mt19937_1998;
const gsl_rng_type *gsl_rng_r250;
const gsl_rng_type *gsl_rng_ran0;
const gsl_rng_type *gsl_rng_ran1;
const gsl_rng_type *gsl_rng_ran2;
const gsl_rng_type *gsl_rng_ran3;
const gsl_rng_type *gsl_rng_rand;
const gsl_rng_type *gsl_rng_rand48;
const gsl_rng_type *gsl_rng_random128_bsd;
const gsl_rng_type *gsl_rng_random128_glibc2;
const gsl_rng_type *gsl_rng_random128_libc5;
const gsl_rng_type *gsl_rng_random256_bsd;
const gsl_rng_type *gsl_rng_random256_glibc2;
const gsl_rng_type *gsl_rng_random256_libc5;
const gsl_rng_type *gsl_rng_random32_bsd;
const gsl_rng_type *gsl_rng_random32_glibc2;
const gsl_rng_type *gsl_rng_random32_libc5;
const gsl_rng_type *gsl_rng_random64_bsd;
const gsl_rng_type *gsl_rng_random64_glibc2;
const gsl_rng_type *gsl_rng_random64_libc5;
const gsl_rng_type *gsl_rng_random8_bsd;
const gsl_rng_type *gsl_rng_random8_glibc2;
const gsl_rng_type *gsl_rng_random8_libc5;
const gsl_rng_type *gsl_rng_random_bsd;
const gsl_rng_type *gsl_rng_random_glibc2;
const gsl_rng_type *gsl_rng_random_libc5;
const gsl_rng_type *gsl_rng_randu;
const gsl_rng_type *gsl_rng_ranf;
const gsl_rng_type *gsl_rng_ranlux;
const gsl_rng_type *gsl_rng_ranlux389;
const gsl_rng_type *gsl_rng_ranlxd1;
const gsl_rng_type *gsl_rng_ranlxd2;
const gsl_rng_type *gsl_rng_ranlxs0;
const gsl_rng_type *gsl_rng_ranlxs1;
const gsl_rng_type *gsl_rng_ranlxs2;
const gsl_rng_type *gsl_rng_ranmar;
const gsl_rng_type *gsl_rng_slatec;
const gsl_rng_type *gsl_rng_taus;
const gsl_rng_type *gsl_rng_taus2;
const gsl_rng_type *gsl_rng_taus113;
const gsl_rng_type *gsl_rng_transputer;
const gsl_rng_type *gsl_rng_tt800;
const gsl_rng_type *gsl_rng_uni;
const gsl_rng_type *gsl_rng_uni32;
const gsl_rng_type *gsl_rng_vax;
const gsl_rng_type *gsl_rng_waterman14;
const gsl_rng_type *gsl_rng_zuf;

const gsl_rng_type *gsl_rng_default;
unsigned long int gsl_rng_default_seed;

gsl_rng *gsl_rng_alloc (const gsl_rng_type * T);
int gsl_rng_memcpy (gsl_rng * dest, const gsl_rng * src);
gsl_rng *gsl_rng_clone (const gsl_rng * r);

void gsl_rng_free (gsl_rng * r);

void gsl_rng_set (const gsl_rng * r, unsigned long int seed);
unsigned long int gsl_rng_max (const gsl_rng * r);
unsigned long int gsl_rng_min (const gsl_rng * r);
const char *gsl_rng_name (const gsl_rng * r);

int gsl_rng_fread (FILE * stream, gsl_rng * r);
int gsl_rng_fwrite (FILE * stream, const gsl_rng * r);

size_t gsl_rng_size (const gsl_rng * r);
void * gsl_rng_state (const gsl_rng * r);

void gsl_rng_print_state (const gsl_rng * r);

const gsl_rng_type * gsl_rng_env_setup (void);

unsigned long int gsl_rng_get (const gsl_rng * r);
double gsl_rng_uniform (const gsl_rng * r);
double gsl_rng_uniform_pos (const gsl_rng * r);
unsigned long int gsl_rng_uniform_int (const gsl_rng * r, unsigned long int n);
