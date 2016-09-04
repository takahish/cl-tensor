%module gsl_vector

%{
#include <stdlib.h>
#include <gsl/gsl_types.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_inline.h>
#include <gsl/gsl_check_range.h>
#include <gsl/gsl_block_double.h>
#include <gsl/gsl_block_float.h>
#include <gsl/gsl_vector_double.h>
#include <gsl/gsl_vector_float.h>
%}

typedef unsigned int size_t;

struct gsl_vector
{
  size_t size;
  size_t stride;
  double *data;
  gsl_block *block;
  int owner;
};

struct gsl_vector_float
{
  size_t size;
  size_t stride;
  float *data;
  gsl_block_float *block;
  int owner;
};

/* Allocation */

gsl_vector *gsl_vector_alloc (const size_t n);
gsl_vector *gsl_vector_calloc (const size_t n);
void gsl_vector_free (gsl_vector * v);

gsl_vector_float *gsl_vector_float_alloc (const size_t n);
gsl_vector_float *gsl_vector_float_calloc (const size_t n);
void gsl_vector_float_free (gsl_vector_float * v);

/* Operations */

void gsl_vector_set_zero (gsl_vector * v);
void gsl_vector_set_all (gsl_vector * v, double x);
int gsl_vector_set_basis (gsl_vector * v, size_t i);

int gsl_vector_memcpy (gsl_vector * dest, const gsl_vector * src);

int gsl_vector_reverse (gsl_vector * v);

int gsl_vector_swap (gsl_vector * v, gsl_vector * w);
int gsl_vector_swap_elements (gsl_vector * v, const size_t i, const size_t j);

double gsl_vector_max (const gsl_vector * v);
double gsl_vector_min (const gsl_vector * v);
void gsl_vector_minmax (const gsl_vector * v, double * min_out, double * max_out);

size_t gsl_vector_max_index (const gsl_vector * v);
size_t gsl_vector_min_index (const gsl_vector * v);
void gsl_vector_minmax_index (const gsl_vector * v, size_t * imin, size_t * imax);

int gsl_vector_add (gsl_vector * a, const gsl_vector * b);
int gsl_vector_sub (gsl_vector * a, const gsl_vector * b);
int gsl_vector_mul (gsl_vector * a, const gsl_vector * b);
int gsl_vector_div (gsl_vector * a, const gsl_vector * b);
int gsl_vector_scale (gsl_vector * a, const double x);
int gsl_vector_add_constant (gsl_vector * a, const double x);

int gsl_vector_equal (const gsl_vector * u, const gsl_vector * v);

int gsl_vector_isnull (const gsl_vector * v);
int gsl_vector_ispos (const gsl_vector * v);
int gsl_vector_isneg (const gsl_vector * v);
int gsl_vector_isnonneg (const gsl_vector * v);

double gsl_vector_get (const gsl_vector * v, const size_t i);
void gsl_vector_set (gsl_vector * v, const size_t i, double x);
double * gsl_vector_ptr (gsl_vector * v, const size_t i);
const double * gsl_vector_const_ptr (const gsl_vector * v, const size_t i);

void gsl_vector_float_set_zero (gsl_vector_float * v);
void gsl_vector_float_set_all (gsl_vector_float * v, float x);
int gsl_vector_float_set_basis (gsl_vector_float * v, size_t i);

int gsl_vector_float_memcpy (gsl_vector_float * dest, const gsl_vector_float * src);

int gsl_vector_float_reverse (gsl_vector_float * v);

int gsl_vector_float_swap (gsl_vector_float * v, gsl_vector_float * w);
int gsl_vector_float_swap_elements (gsl_vector_float * v, const size_t i, const size_t j);

float gsl_vector_float_max (const gsl_vector_float * v);
float gsl_vector_float_min (const gsl_vector_float * v);
void gsl_vector_float_minmax (const gsl_vector_float * v, float * min_out, float * max_out);

size_t gsl_vector_float_max_index (const gsl_vector_float * v);
size_t gsl_vector_float_min_index (const gsl_vector_float * v);
void gsl_vector_float_minmax_index (const gsl_vector_float * v, size_t * imin, size_t * imax);

int gsl_vector_float_add (gsl_vector_float * a, const gsl_vector_float * b);
int gsl_vector_float_sub (gsl_vector_float * a, const gsl_vector_float * b);
int gsl_vector_float_mul (gsl_vector_float * a, const gsl_vector_float * b);
int gsl_vector_float_div (gsl_vector_float * a, const gsl_vector_float * b);
int gsl_vector_float_scale (gsl_vector_float * a, const double x);
int gsl_vector_float_add_constant (gsl_vector_float * a, const double x);

int gsl_vector_float_equal (const gsl_vector_float * u, const gsl_vector_float * v);

int gsl_vector_float_isnull (const gsl_vector_float * v);
int gsl_vector_float_ispos (const gsl_vector_float * v);
int gsl_vector_float_isneg (const gsl_vector_float * v);
int gsl_vector_float_isnonneg (const gsl_vector_float * v);

float gsl_vector_float_get (const gsl_vector_float * v, const size_t i);
void gsl_vector_float_set (gsl_vector_float * v, const size_t i, float x);
float * gsl_vector_float_ptr (gsl_vector_float * v, const size_t i);
const float * gsl_vector_float_const_ptr (const gsl_vector_float * v, const size_t i);
