%module gsl_histogram

%{
#include <stdlib.h>
#include <stdio.h>
#include <gsl/gsl_histogram.h>
%}

typedef unsigned int size_t;

struct gsl_histogram
{
  size_t n ;
  double * range ;
  double * bin ;
};

struct gsl_histogram_pdf {
  size_t n ;
  double * range ;
  double * sum ;
};


/* histogram allocation */

gsl_histogram * gsl_histogram_alloc (size_t n);
int gsl_histogram_set_ranges (gsl_histogram * h, const double range[], size_t size);
int gsl_histogram_set_ranges_uniform (gsl_histogram * h, double xmin, double xmax);
void gsl_histogram_free (gsl_histogram * h);


/* copying histograms */

int gsl_histogram_memcpy(gsl_histogram * dest, const gsl_histogram * source);
gsl_histogram * gsl_histogram_clone(const gsl_histogram * source);


/* updating and accessing histogram elements */

int gsl_histogram_increment (gsl_histogram * h, double x);
int gsl_histogram_accumulate (gsl_histogram * h, double x, double weight);
double gsl_histogram_get (const gsl_histogram * h, size_t i);
int gsl_histogram_get_range (const gsl_histogram * h, size_t i, double * lower, double * upper);
double gsl_histogram_max (const gsl_histogram * h);
double gsl_histogram_min (const gsl_histogram * h);
size_t gsl_histogram_bins (const gsl_histogram * h);
void gsl_histogram_reset (gsl_histogram * h);


/* searching histogram ranges */

int gsl_histogram_find (const gsl_histogram * h, const double x, size_t * i);


/* histogram statistics */

double gsl_histogram_max_val (const gsl_histogram * h);
size_t gsl_histogram_max_bin (const gsl_histogram * h);
double gsl_histogram_min_val (const gsl_histogram * h);
size_t gsl_histogram_min_bin (const gsl_histogram * h);
double gsl_histogram_mean (const gsl_histogram * h);
double gsl_histogram_sigma (const gsl_histogram * h);
double gsl_histogram_sum (const gsl_histogram * h);


/* histogram operations */

int gsl_histogram_equal_bins_p(const gsl_histogram *h1, const gsl_histogram *h2);
int gsl_histogram_add(gsl_histogram *h1, const gsl_histogram *h2);
int gsl_histogram_sub(gsl_histogram *h1, const gsl_histogram *h2);
int gsl_histogram_mul(gsl_histogram *h1, const gsl_histogram *h2);
int gsl_histogram_div(gsl_histogram *h1, const gsl_histogram *h2);
int gsl_histogram_scale(gsl_histogram *h, double scale);
int gsl_histogram_shift (gsl_histogram * h, double shift);


/* the histogram probability distribution struct */

gsl_histogram_pdf * gsl_histogram_pdf_alloc (const size_t n);
int gsl_histogram_pdf_init (gsl_histogram_pdf * p, const gsl_histogram * h);
void gsl_histogram_pdf_free (gsl_histogram_pdf * p);
double gsl_histogram_pdf_sample (const gsl_histogram_pdf * p, double r);
