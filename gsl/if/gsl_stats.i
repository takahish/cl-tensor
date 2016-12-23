%module gsl_stats

%{
#include <stddef.h>
%}

typedef unsigned int size_t;

double gsl_stats_mean (const double data[], const size_t stride, const size_t n);
double gsl_stats_variance (const double data[], const size_t stride, const size_t n);
double gsl_stats_sd (const double data[], const size_t stride, const size_t n);
double gsl_stats_variance_with_fixed_mean (const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_sd_with_fixed_mean (const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_tss (const double data[], const size_t stride, const size_t n);
double gsl_stats_tss_m (const double data[], const size_t stride, const size_t n, const double mean);

double gsl_stats_absdev (const double data[], const size_t stride, const size_t n);
double gsl_stats_skew (const double data[], const size_t stride, const size_t n);
double gsl_stats_kurtosis (const double data[], const size_t stride, const size_t n);
double gsl_stats_lag1_autocorrelation (const double data[], const size_t stride, const size_t n);

double gsl_stats_covariance (const double data1[], const size_t stride1,const double data2[], const size_t stride2, const size_t n);
double gsl_stats_correlation (const double data1[], const size_t stride1,const double data2[], const size_t stride2, const size_t n);
double gsl_stats_spearman (const double data1[], const size_t stride1, const double data2[], const size_t stride2, const size_t n, double work[]);

double gsl_stats_variance_m (const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_sd_m (const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_absdev_m (const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_skew_m_sd (const double data[], const size_t stride, const size_t n, const double mean, const double sd);
double gsl_stats_kurtosis_m_sd (const double data[], const size_t stride, const size_t n, const double mean, const double sd);
double gsl_stats_lag1_autocorrelation_m (const double data[], const size_t stride, const size_t n, const double mean);

double gsl_stats_covariance_m (const double data1[], const size_t stride1,const double data2[], const size_t stride2, const size_t n, const double mean1, const double mean2);

double gsl_stats_float_mean (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_variance (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_sd (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_variance_with_fixed_mean (const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_sd_with_fixed_mean (const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_tss (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_tss_m (const float data[], const size_t stride, const size_t n, const double mean);

double gsl_stats_float_absdev (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_skew (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_kurtosis (const float data[], const size_t stride, const size_t n);
double gsl_stats_float_lag1_autocorrelation (const float data[], const size_t stride, const size_t n);

double gsl_stats_float_covariance (const float data1[], const size_t stride1,const float data2[], const size_t stride2, const size_t n);
double gsl_stats_float_correlation (const float data1[], const size_t stride1,const float data2[], const size_t stride2, const size_t n);
double gsl_stats_float_spearman (const float data1[], const size_t stride1, const float data2[], const size_t stride2, const size_t n, double work[]);

double gsl_stats_float_variance_m (const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_sd_m (const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_absdev_m (const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_skew_m_sd (const float data[], const size_t stride, const size_t n, const double mean, const double sd);
double gsl_stats_float_kurtosis_m_sd (const float data[], const size_t stride, const size_t n, const double mean, const double sd);
double gsl_stats_float_lag1_autocorrelation_m (const float data[], const size_t stride, const size_t n, const double mean);

double gsl_stats_float_covariance_m (const float data1[], const size_t stride1,const float data2[], const size_t stride2, const size_t n, const double mean1, const double mean2);

/* DEFINED FOR FLOATING POINT TYPES ONLY */

double gsl_stats_wmean (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);
double gsl_stats_wvariance (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);
double gsl_stats_wsd (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);
double gsl_stats_wvariance_with_fixed_mean (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_wsd_with_fixed_mean (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_wtss (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);
double gsl_stats_wtss_m (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_wabsdev (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);
double gsl_stats_wskew (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);
double gsl_stats_wkurtosis (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n);

double gsl_stats_wvariance_m (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_wsd_m (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_wabsdev_m (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_wskew_m_sd (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double wmean, const double wsd);
double gsl_stats_wkurtosis_m_sd (const double w[], const size_t wstride, const double data[], const size_t stride, const size_t n, const double wmean, const double wsd);

double gsl_stats_float_wmean (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);
double gsl_stats_float_wvariance (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);
double gsl_stats_float_wsd (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);
double gsl_stats_float_wvariance_with_fixed_mean (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_wsd_with_fixed_mean (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double mean);
double gsl_stats_float_wtss (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);
double gsl_stats_float_wtss_m (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_float_wabsdev (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);
double gsl_stats_float_wskew (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);
double gsl_stats_float_wkurtosis (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n);

double gsl_stats_float_wvariance_m (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_float_wsd_m (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_float_wabsdev_m (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double wmean);
double gsl_stats_float_wskew_m_sd (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double wmean, const double wsd);
double gsl_stats_float_wkurtosis_m_sd (const float w[], const size_t wstride, const float data[], const size_t stride, const size_t n, const double wmean, const double wsd);

/* END OF FLOATING POINT TYPES */

double gsl_stats_pvariance (const double data1[], const size_t stride1, const size_t n1, const double data2[], const size_t stride2, const size_t n2);
double gsl_stats_ttest (const double data1[], const size_t stride1, const size_t n1, const double data2[], const size_t stride2, const size_t n2);

double gsl_stats_max (const double data[], const size_t stride, const size_t n);
double gsl_stats_min (const double data[], const size_t stride, const size_t n);
void gsl_stats_minmax (double * min, double * max, const double data[], const size_t stride, const size_t n);

size_t gsl_stats_max_index (const double data[], const size_t stride, const size_t n);
size_t gsl_stats_min_index (const double data[], const size_t stride, const size_t n);
void gsl_stats_minmax_index (size_t * min_index, size_t * max_index, const double data[], const size_t stride, const size_t n);

double gsl_stats_median_from_sorted_data (const double sorted_data[], const size_t stride, const size_t n) ;
double gsl_stats_quantile_from_sorted_data (const double sorted_data[], const size_t stride, const size_t n, const double f) ;

double gsl_stats_float_pvariance (const float data1[], const size_t stride1, const size_t n1, const float data2[], const size_t stride2, const size_t n2);
double gsl_stats_float_ttest (const float data1[], const size_t stride1, const size_t n1, const float data2[], const size_t stride2, const size_t n2);

float gsl_stats_float_max (const float data[], const size_t stride, const size_t n);
float gsl_stats_float_min (const float data[], const size_t stride, const size_t n);
void gsl_stats_float_minmax (float * min, float * max, const float data[], const size_t stride, const size_t n);

size_t gsl_stats_float_max_index (const float data[], const size_t stride, const size_t n);
size_t gsl_stats_float_min_index (const float data[], const size_t stride, const size_t n);
void gsl_stats_float_minmax_index (size_t * min_index, size_t * max_index, const float data[], const size_t stride, const size_t n);

double gsl_stats_float_median_from_sorted_data (const float sorted_data[], const size_t stride, const size_t n) ;
double gsl_stats_float_quantile_from_sorted_data (const float sorted_data[], const size_t stride, const size_t n, const double f) ;
