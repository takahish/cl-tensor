%module gsl_matrix

%{
#include <stdlib.h>
#include <gsl/gsl_types.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_inline.h>
#include <gsl/gsl_check_range.h>
#include <gsl/gsl_vector_double.h>
#include <gsl/gsl_vector_float.h>
#include <gsl/gsl_vector_int.h>
#include <gsl/gsl_vector_uint.h>
#include <gsl/gsl_matrix_double.h>
#include <gsl/gsl_matrix_float.h>
#include <gsl/gsl_matrix_int.h>
#include <gsl/gsl_matrix_uint.h>
%}

typedef unsigned int size_t;

struct gsl_matrix
{
  size_t size1;
  size_t size2;
  size_t tda;
  double * data;
  gsl_block * block;
  int owner;
};

struct gsl_matrix_float
{
  size_t size1;
  size_t size2;
  size_t tda;
  float * data;
  gsl_block_float * block;
  int owner;
};

struct gsl_matrix_int
{
  size_t size1;
  size_t size2;
  size_t tda;
  int * data;
  gsl_block_int * block;
  int owner;
};

struct gsl_matrix_uint
{
  size_t size1;
  size_t size2;
  size_t tda;
  unsigned int * data;
  gsl_block_uint * block;
  int owner;
};

/* Allocation */

gsl_matrix *
gsl_matrix_alloc (const size_t n1, const size_t n2);

gsl_matrix *
gsl_matrix_calloc (const size_t n1, const size_t n2);

void gsl_matrix_free (gsl_matrix * m);

gsl_matrix_float *
gsl_matrix_float_alloc (const size_t n1, const size_t n2);

gsl_matrix_float *
gsl_matrix_float_calloc (const size_t n1, const size_t n2);

void gsl_matrix_float_free (gsl_matrix_float * m);

gsl_matrix_int *
gsl_matrix_int_alloc (const size_t n1, const size_t n2);

gsl_matrix_int *
gsl_matrix_int_calloc (const size_t n1, const size_t n2);

void gsl_matrix_int_free (gsl_matrix_int * m);

gsl_matrix_uint *
gsl_matrix_uint_alloc (const size_t n1, const size_t n2);

gsl_matrix_uint *
gsl_matrix_uint_calloc (const size_t n1, const size_t n2);

void gsl_matrix_uint_free (gsl_matrix_uint * m);

/* Operations */

void gsl_matrix_set_zero (gsl_matrix * m);
void gsl_matrix_set_identity (gsl_matrix * m);
void gsl_matrix_set_all (gsl_matrix * m, double x);

int gsl_matrix_memcpy(gsl_matrix * dest, const gsl_matrix * src);
int gsl_matrix_swap(gsl_matrix * m1, gsl_matrix * m2);

int gsl_matrix_swap_rows(gsl_matrix * m, const size_t i, const size_t j);
int gsl_matrix_swap_columns(gsl_matrix * m, const size_t i, const size_t j);
int gsl_matrix_swap_rowcol(gsl_matrix * m, const size_t i, const size_t j);
int gsl_matrix_transpose (gsl_matrix * m);
int gsl_matrix_transpose_memcpy (gsl_matrix * dest, const gsl_matrix * src);

double gsl_matrix_max (const gsl_matrix * m);
double gsl_matrix_min (const gsl_matrix * m);
void gsl_matrix_minmax (const gsl_matrix * m, double * min_out, double * max_out);

void gsl_matrix_max_index (const gsl_matrix * m, size_t * imax, size_t *jmax);
void gsl_matrix_min_index (const gsl_matrix * m, size_t * imin, size_t *jmin);
void gsl_matrix_minmax_index (const gsl_matrix * m, size_t * imin, size_t * jmin, size_t * imax, size_t * jmax);

int gsl_matrix_equal (const gsl_matrix * a, const gsl_matrix * b);

int gsl_matrix_isnull (const gsl_matrix * m);
int gsl_matrix_ispos (const gsl_matrix * m);
int gsl_matrix_isneg (const gsl_matrix * m);
int gsl_matrix_isnonneg (const gsl_matrix * m);

int gsl_matrix_add (gsl_matrix * a, const gsl_matrix * b);
int gsl_matrix_sub (gsl_matrix * a, const gsl_matrix * b);
int gsl_matrix_mul_elements (gsl_matrix * a, const gsl_matrix * b);
int gsl_matrix_div_elements (gsl_matrix * a, const gsl_matrix * b);
int gsl_matrix_scale (gsl_matrix * a, const double x);
int gsl_matrix_add_constant (gsl_matrix * a, const double x);
int gsl_matrix_add_diagonal (gsl_matrix * a, const double x);

int gsl_matrix_get_row(gsl_vector * v, const gsl_matrix * m, const size_t i);
int gsl_matrix_get_col(gsl_vector * v, const gsl_matrix * m, const size_t j);
int gsl_matrix_set_row(gsl_matrix * m, const size_t i, const gsl_vector * v);
int gsl_matrix_set_col(gsl_matrix * m, const size_t j, const gsl_vector * v);

double gsl_matrix_get(const gsl_matrix * m, const size_t i, const size_t j);
void gsl_matrix_set(gsl_matrix * m, const size_t i, const size_t j, const double x);
double * gsl_matrix_ptr(gsl_matrix * m, const size_t i, const size_t j);

void gsl_matrix_float_set_zero (gsl_matrix_float * m);
void gsl_matrix_float_set_identity (gsl_matrix_float * m);
void gsl_matrix_float_set_all (gsl_matrix_float * m, float x);

int gsl_matrix_float_memcpy(gsl_matrix_float * dest, const gsl_matrix_float * src);
int gsl_matrix_float_swap(gsl_matrix_float * m1, gsl_matrix_float * m2);

int gsl_matrix_float_swap_rows(gsl_matrix_float * m, const size_t i, const size_t j);
int gsl_matrix_float_swap_columns(gsl_matrix_float * m, const size_t i, const size_t j);
int gsl_matrix_float_swap_rowcol(gsl_matrix_float * m, const size_t i, const size_t j);
int gsl_matrix_float_transpose (gsl_matrix_float * m);
int gsl_matrix_float_transpose_memcpy (gsl_matrix_float * dest, const gsl_matrix_float * src);

float gsl_matrix_float_max (const gsl_matrix_float * m);
float gsl_matrix_float_min (const gsl_matrix_float * m);
void gsl_matrix_float_minmax (const gsl_matrix_float * m, float * min_out, float * max_out);

void gsl_matrix_float_max_index (const gsl_matrix_float * m, size_t * imax, size_t *jmax);
void gsl_matrix_float_min_index (const gsl_matrix_float * m, size_t * imin, size_t *jmin);
void gsl_matrix_float_minmax_index (const gsl_matrix_float * m, size_t * imin, size_t * jmin, size_t * imax, size_t * jmax);

int gsl_matrix_float_equal (const gsl_matrix_float * a, const gsl_matrix_float * b);

int gsl_matrix_float_isnull (const gsl_matrix_float * m);
int gsl_matrix_float_ispos (const gsl_matrix_float * m);
int gsl_matrix_float_isneg (const gsl_matrix_float * m);
int gsl_matrix_float_isnonneg (const gsl_matrix_float * m);

int gsl_matrix_float_add (gsl_matrix_float * a, const gsl_matrix_float * b);
int gsl_matrix_float_sub (gsl_matrix_float * a, const gsl_matrix_float * b);
int gsl_matrix_float_mul_elements (gsl_matrix_float * a, const gsl_matrix_float * b);
int gsl_matrix_float_div_elements (gsl_matrix_float * a, const gsl_matrix_float * b);
int gsl_matrix_float_scale (gsl_matrix_float * a, const double x);
int gsl_matrix_float_add_constant (gsl_matrix_float * a, const double x);
int gsl_matrix_float_add_diagonal (gsl_matrix_float * a, const double x);

int gsl_matrix_float_get_row(gsl_vector_float * v, const gsl_matrix_float * m, const size_t i);
int gsl_matrix_float_get_col(gsl_vector_float * v, const gsl_matrix_float * m, const size_t j);
int gsl_matrix_float_set_row(gsl_matrix_float * m, const size_t i, const gsl_vector_float * v);
int gsl_matrix_float_set_col(gsl_matrix_float * m, const size_t j, const gsl_vector_float * v);

float   gsl_matrix_float_get(const gsl_matrix_float * m, const size_t i, const size_t j);
void    gsl_matrix_float_set(gsl_matrix_float * m, const size_t i, const size_t j, const float x);
float * gsl_matrix_float_ptr(gsl_matrix_float * m, const size_t i, const size_t j);

void gsl_matrix_int_set_zero (gsl_matrix_int * m);
void gsl_matrix_int_set_identity (gsl_matrix_int * m);
void gsl_matrix_int_set_all (gsl_matrix_int * m, int x);

int gsl_matrix_int_memcpy(gsl_matrix_int * dest, const gsl_matrix_int * src);
int gsl_matrix_int_swap(gsl_matrix_int * m1, gsl_matrix_int * m2);

int gsl_matrix_int_swap_rows(gsl_matrix_int * m, const size_t i, const size_t j);
int gsl_matrix_int_swap_columns(gsl_matrix_int * m, const size_t i, const size_t j);
int gsl_matrix_int_swap_rowcol(gsl_matrix_int * m, const size_t i, const size_t j);
int gsl_matrix_int_transpose (gsl_matrix_int * m);
int gsl_matrix_int_transpose_memcpy (gsl_matrix_int * dest, const gsl_matrix_int * src);

int gsl_matrix_int_max (const gsl_matrix_int * m);
int gsl_matrix_int_min (const gsl_matrix_int * m);
void gsl_matrix_int_minmax (const gsl_matrix_int * m, int * min_out, int * max_out);

void gsl_matrix_int_max_index (const gsl_matrix_int * m, size_t * imax, size_t *jmax);
void gsl_matrix_int_min_index (const gsl_matrix_int * m, size_t * imin, size_t *jmin);
void gsl_matrix_int_minmax_index (const gsl_matrix_int * m, size_t * imin, size_t * jmin, size_t * imax, size_t * jmax);

int gsl_matrix_int_equal (const gsl_matrix_int * a, const gsl_matrix_int * b);

int gsl_matrix_int_isnull (const gsl_matrix_int * m);
int gsl_matrix_int_ispos (const gsl_matrix_int * m);
int gsl_matrix_int_isneg (const gsl_matrix_int * m);
int gsl_matrix_int_isnonneg (const gsl_matrix_int * m);

int gsl_matrix_int_add (gsl_matrix_int * a, const gsl_matrix_int * b);
int gsl_matrix_int_sub (gsl_matrix_int * a, const gsl_matrix_int * b);
int gsl_matrix_int_mul_elements (gsl_matrix_int * a, const gsl_matrix_int * b);
int gsl_matrix_int_div_elements (gsl_matrix_int * a, const gsl_matrix_int * b);
int gsl_matrix_int_scale (gsl_matrix_int * a, const double x);
int gsl_matrix_int_add_constant (gsl_matrix_int * a, const double x);
int gsl_matrix_int_add_diagonal (gsl_matrix_int * a, const double x);

int gsl_matrix_int_get_row(gsl_vector_int * v, const gsl_matrix_int * m, const size_t i);
int gsl_matrix_int_get_col(gsl_vector_int * v, const gsl_matrix_int * m, const size_t j);
int gsl_matrix_int_set_row(gsl_matrix_int * m, const size_t i, const gsl_vector_int * v);
int gsl_matrix_int_set_col(gsl_matrix_int * m, const size_t j, const gsl_vector_int * v);

int gsl_matrix_int_get(const gsl_matrix_int * m, const size_t i, const size_t j);
void gsl_matrix_int_set(gsl_matrix_int * m, const size_t i, const size_t j, const int x);
int * gsl_matrix_int_ptr(gsl_matrix_int * m, const size_t i, const size_t j);

void gsl_matrix_uint_set_zero (gsl_matrix_uint * m);
void gsl_matrix_uint_set_identity (gsl_matrix_uint * m);
void gsl_matrix_uint_set_all (gsl_matrix_uint * m, unsigned int x);

int gsl_matrix_uint_memcpy(gsl_matrix_uint * dest, const gsl_matrix_uint * src);
int gsl_matrix_uint_swap(gsl_matrix_uint * m1, gsl_matrix_uint * m2);

int gsl_matrix_uint_swap_rows(gsl_matrix_uint * m, const size_t i, const size_t j);
int gsl_matrix_uint_swap_columns(gsl_matrix_uint * m, const size_t i, const size_t j);
int gsl_matrix_uint_swap_rowcol(gsl_matrix_uint * m, const size_t i, const size_t j);
int gsl_matrix_uint_transpose (gsl_matrix_uint * m);
int gsl_matrix_uint_transpose_memcpy (gsl_matrix_uint * dest, const gsl_matrix_uint * src);

unsigned int gsl_matrix_uint_max (const gsl_matrix_uint * m);
unsigned int gsl_matrix_uint_min (const gsl_matrix_uint * m);
void gsl_matrix_uint_minmax (const gsl_matrix_uint * m, unsigned int * min_out, unsigned int * max_out);

void gsl_matrix_uint_max_index (const gsl_matrix_uint * m, size_t * imax, size_t *jmax);
void gsl_matrix_uint_min_index (const gsl_matrix_uint * m, size_t * imin, size_t *jmin);
void gsl_matrix_uint_minmax_index (const gsl_matrix_uint * m, size_t * imin, size_t * jmin, size_t * imax, size_t * jmax);

int gsl_matrix_uint_equal (const gsl_matrix_uint * a, const gsl_matrix_uint * b);

int gsl_matrix_uint_isnull (const gsl_matrix_uint * m);
int gsl_matrix_uint_ispos (const gsl_matrix_uint * m);
int gsl_matrix_uint_isneg (const gsl_matrix_uint * m);
int gsl_matrix_uint_isnonneg (const gsl_matrix_uint * m);

int gsl_matrix_uint_add (gsl_matrix_uint * a, const gsl_matrix_uint * b);
int gsl_matrix_uint_sub (gsl_matrix_uint * a, const gsl_matrix_uint * b);
int gsl_matrix_uint_mul_elements (gsl_matrix_uint * a, const gsl_matrix_uint * b);
int gsl_matrix_uint_div_elements (gsl_matrix_uint * a, const gsl_matrix_uint * b);
int gsl_matrix_uint_scale (gsl_matrix_uint * a, const double x);
int gsl_matrix_uint_add_constant (gsl_matrix_uint * a, const double x);
int gsl_matrix_uint_add_diagonal (gsl_matrix_uint * a, const double x);

int gsl_matrix_uint_get_row(gsl_vector_uint * v, const gsl_matrix_uint * m, const size_t i);
int gsl_matrix_uint_get_col(gsl_vector_uint * v, const gsl_matrix_uint * m, const size_t j);
int gsl_matrix_uint_set_row(gsl_matrix_uint * m, const size_t i, const gsl_vector_uint * v);
int gsl_matrix_uint_set_col(gsl_matrix_uint * m, const size_t j, const gsl_vector_uint * v);

unsigned int gsl_matrix_uint_get(const gsl_matrix_uint * m, const size_t i, const size_t j);
void gsl_matrix_uint_set(gsl_matrix_uint * m, const size_t i, const size_t j, const unsigned int x);
unsigned int * gsl_matrix_uint_ptr(gsl_matrix_uint * m, const size_t i, const size_t j);
