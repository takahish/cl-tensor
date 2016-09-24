%module gsl_sort

%{
%}

typedef unsigned int size_t;

typedef int (*gsl_comparison_fn_t) (const void *, const void *);

void gsl_heapsort (void * array, size_t count, size_t size, gsl_comparison_fn_t compare);
int gsl_heapsort_index (size_t * p, const void * array, size_t count, size_t size, gsl_comparison_fn_t compare);
