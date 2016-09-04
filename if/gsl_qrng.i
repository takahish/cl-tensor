%module gsl_qrng

%{
#include <stdlib.h>
#include <gsl/gsl_types.h>
#include <gsl/gsl_inline.h>
#include <gsl/gsl_qrng.h>
%}

typedef unsigned int size_t;

struct gsl_qrng_type
{
  const char * name;
  unsigned int max_dimension;
  size_t (*state_size) (unsigned int dimension);
  int (*init_state) (void * state, unsigned int dimension);
  int (*get) (void * state, unsigned int dimension, double x[]);
};

/* Structure describing a generator instance of a
 * specified type, with generator-specific state info
 * and dimension-specific info.
 */
struct gsl_qrng
{
  const gsl_qrng_type * type;
  unsigned int dimension;
  size_t state_size;
  void * state;
};

/* Supported generator types.
 */
const gsl_qrng_type * gsl_qrng_niederreiter_2;
const gsl_qrng_type * gsl_qrng_sobol;
const gsl_qrng_type * gsl_qrng_halton;
const gsl_qrng_type * gsl_qrng_reversehalton;

/* Allocate and initialize a generator
 * of the specified type, in the given
 * space dimension.
 */
gsl_qrng * gsl_qrng_alloc (const gsl_qrng_type * T, unsigned int dimension);


/* Copy a generator. */
int gsl_qrng_memcpy (gsl_qrng * dest, const gsl_qrng * src);


/* Clone a generator. */
gsl_qrng * gsl_qrng_clone (const gsl_qrng * q);


/* Free a generator. */
void gsl_qrng_free (gsl_qrng * q);


/* Intialize a generator. */
void gsl_qrng_init (gsl_qrng * q);


/* Get the standardized name of the generator. */
const char * gsl_qrng_name (const gsl_qrng * q);


/* ISN'T THIS CONFUSING FOR PEOPLE?
  WHAT IF SOMEBODY TRIES TO COPY WITH THIS ???
  */
size_t gsl_qrng_size (const gsl_qrng * q);


void * gsl_qrng_state (const gsl_qrng * q);


/* Retrieve next vector in sequence. */
int gsl_qrng_get (const gsl_qrng * q, double x[]);
