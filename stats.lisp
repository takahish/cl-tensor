;;;; cl-gsl/stats.lisp
;;;;
;;;; This file describes the statistical functions in the library. The basic
;;;; statistical functions include routines to compute the mean, variance and
;;;; standard deviation. More advanced functions allow you to calculate absolute
;;;; deviations, skewness, and kurtosis as well as the median and arbitrary
;;;; pecentiles. The algorithms use recurrence relations to compute average
;;;; quantities in a stable way, without large intermediate values that might
;;;; overflow.

;;;; Copyright (C) 2016 Takahiro Ishikawa
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see http://www.gnu.org/licenses/.

(cl:in-package "GSL")

(defgeneric stats-mean (data stride n)
  (:documentation
   "This function returns the arithmetic mean of data, a dataset of length n with stride."))

(defmethod stats-mean ((data vector-double) stride n)
  (gsl_stats_mean (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-mean ((data vector-float) stride n)
  (gsl_stats_float_mean (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-variance (data stride n)
  (:documentation
   "This function returns the estimated, or sample variance of data, a dataset of length
n with stride."))

(defmethod stats-variance ((data vector-double) stride n)
  (gsl_stats_variance (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-variance ((data vector-float) stride n)
  (gsl_stats_float_variance (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-variance-m (data stride n mean)
  (:documentation
   "This function returns the sample variance of data relative to the given value of mean."))

(defmethod stats-variance-m ((data vector-double) stride n mean)
  (gsl_stats_variance_m (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-variance-m ((data vector-float) stride n mean)
  (gsl_stats_float_variance_m (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-sd (data stride n)
  (:documentation
   "The standard deviation is defined as the square root of the variance. This function
return the square root of the corresponding variance function."))

(defmethod stats-sd ((data vector-double) stride n)
  (gsl_stats_sd (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-sd ((data vector-float) stride n)
  (gsl_stats_float_sd (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-sd-m (data stride n mean)
  (:documentation
   "The standard deviation is defined as the square root of the variance. This function
return the square root of the corresponding variance function."))

(defmethod stats-sd-m ((data vector-double) stride n mean)
  (gsl_stats_sd_m (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-sd-m ((data vector-float) stride n mean)
  (gsl_stats_float_sd_m (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-tss (data stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about the mean."))

(defmethod stats-tss ((data vector-double) stride n)
  (gsl_stats_tss (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-tss ((data vector-float) stride n)
  (gsl_stats_float_tss (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-tss-m (data stride n mean)
  (:documentation
   "This function return the total sum of squared (TSS) of data about the mean."))

(defmethod stats-tss-m ((data vector-double) stride n mean)
  (gsl_stats_tss_m (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-tss-m ((data vector-float) stride n mean)
  (gsl_stats_float_tss_m (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-variance-with-fixed-mean (data stride n mean)
  (:documentation
   "This function computes an unbiased estimate of the variance of data when the
population mean of the underlying distribution is known a priori. In this case
the estimator for the variance uses the factor 1/N."))

(defmethod stats-variance-with-fixed-mean ((data vector-double) stride n mean)
  (gsl_stats_variance_with_fixed_mean (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-variance-with-fixed-mean ((data vector-float) stride n mean)
  (gsl_stats_float_variance_with_fixed_mean (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-sd-with-fixed-mean (data stride n mean)
  (:documentation
   "This function calculates the standard deviation of data for a fixed population mean
The result is the square root of the corresponding variance function."))

(defmethod stats-sd-with-fixed-mean ((data vector-double) stride n mean)
  (gsl_stats_sd_with_fixed_mean (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-sd-with-fixed-mean ((data vector-float) stride n mean)
  (gsl_stats_float_sd_with_fixed_mean (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-absdev (data stride n)
  (:documentation
   "This function computes the absoute deviation from the mean of data, a dataset of
length n with stride."))

(defmethod stats-absdev ((data vector-double) stride n)
  (gsl_stats_absdev (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-absdev ((data vector-float) stride n)
  (gsl_stats_float_absdev (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-absdev-m (data stride n mean)
  (:documentation
   "This function computes the absolute deviation of the dataset data relative to the
given value of mean. This function is useful if you have already computed the mean
of data (and want to avoid recomputing it), or wish to calculate the absolute deviation
relative to another value (such as zero, or the median)."))

(defmethod stats-absdev-m ((data vector-double) stride n mean)
  (gsl_stats_absdev_m (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-absdev-m ((data vector-float) stride n mean)
  (gsl_stats_float_absdev_m (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-skew (data stride n)
  (:documentation
   "This function computes the skewness of data, a dataset of length n with stride.
The function computes the mean and estimated standard deviation of data via calls
to gsl_stats_mean and gsl_stats_sd."))

(defmethod stats-skew ((data vector-double) stride n)
  (gsl_stats_skew (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-skew ((data vector-float) stride n)
  (gsl_stats_float_skew (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-skew-m-sd (data stride n mean sd)
  (:documentation
   "This function computes the skewness of the dataset data using the given values
of the mean and standard deviation. This function are usefull if you have already
computed the mean and standard deviation of data and want to avoid recomputing them."))

(defmethod stats-skew-m-sd ((data vector-double) stride n mean sd)
  (gsl_stats_skew_m_sd (gsl_vector_ptr (entity data) 0) stride n mean sd))

(defmethod stats-skew-m-sd ((data vector-float) stride n mean sd)
  (gsl_stats_float_skew_m_sd (gsl_vector_float_ptr (entity data) 0) stride n mean sd))

(defgeneric stats-kurtosis (data stride n)
  (:documentation
   "This function computes the kurtosis of data, a dataset of length n with stride.
The kurtosis measures how sharply peaked a distribution is, relative to its width.
The kurtosis is normalized to zero for a Gaussian distribution."))

(defmethod stats-kurtosis ((data vector-double) stride n)
  (gsl_stats_kurtosis (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-kurtosis ((data vector-float) stride n)
  (gsl_stats_float_kurtosis (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-kurtosis-m-sd (data stride n mean sd)
  (:documentation
   "This function computes the kurtosis of the dataset data using the given values of
the mean and standard deviation sd. This function is useful if you have already
computed the mean and standard deviation of data and want to avoid recomputing them."))

(defmethod stats-kurtosis-m-sd ((data vector-double) stride n mean sd)
  (gsl_stats_kurtosis_m_sd (gsl_vector_ptr (entity data) 0) stride n mean sd))

(defmethod stats-kurtosis-m-sd ((data vector-float) stride n mean sd)
  (gsl_stats_float_kurtosis_m_sd (gsl_vector_float_ptr (entity data) 0) stride n mean sd))

(defgeneric stats-lag1-autocorrelation (data stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset data."))

(defmethod stats-lag1-autocorrelation ((data vector-double) stride n)
  (gsl_stats_lag1_autocorrelation (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-lag1-autocorrelation ((data vector-float) stride n)
  (gsl_stats_float_lag1_autocorrelation (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-lag1-autocorrelation-m (data stride n mean)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset data using the
given value of the mean."))

(defmethod stats-lag1-autocorrelation-m ((data vector-double) stride n mean)
  (gsl_stats_lag1_autocorrelation_m (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-lag1-autocorrelation-m ((data vector-float) stride n mean)
  (gsl_stats_float_lag1_autocorrelation_m (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-covariance (data1 stride1 data2 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets data1 and data2 which must
both be of the same length n."))

(defmethod stats-covariance ((data1 vector-double) stride1 (data2 vector-double) stride2 n)
  (gsl_stats_covariance (gsl_vector_ptr (entity data1) 0) stride1
                        (gsl_vector_ptr (entity data2) 0) stride2 n))

(defmethod stats-covariance ((data1 vector-float) stride1 (data2 vector-float) stride2 n)
  (gsl_stats_float_covariance (gsl_vector_float_ptr (entity data1) 0) stride1
                              (gsl_vector_float_ptr (entity data2) 0) stride2 n))

(defgeneric stats-covariance-m (data1 stride1 data2 stride2 n mean1 mean2)
  (:documentation
   "This function computes the covariance of the datasets data1 and data2 using the
given values of the means, mean1 and mean2. This is useful if you have already
computed the mean of data1 and data2 and want to avoid recomputing them."))

(defmethod stats-covariance-m ((data1 vector-double) stride1
                               (data2 vector-double) stride2
                               n mean1 mean2)
  (gsl_stats_covariance_m (gsl_vector_ptr (entity data1) 0) stride1
                          (gsl_vector_ptr (entity data2) 0) stride2
                          n mean1 mean2))

(defmethod stats-covariance-m ((data1 vector-float) stride1
                               (data2 vector-float) stride2
                               n mean1 mean2)
  (gsl_stats_float_covariance_m (gsl_vector_float_ptr (entity data1) 0) stride1
                                (gsl_vector_float_ptr (entity data2) 0) stride2
                                n mean1 mean2))

(defgeneric stats-correlation (data1 stride1 data2 stride2 n)
  (:documentation
   "This function efficiently computes the Pearson correlation coefficient between the
datasets data1 and data2 which must both be of the same length n."))

(defmethod stats-correlation ((data1 vector-double) stride1 (data2 vector-double) stride2 n)
  (gsl_stats_correlation (gsl_vector_ptr (entity data1) 0) stride1
                         (gsl_vector_ptr (entity data2) 0) stride2 n))

(defmethod stats-correlation ((data1 vector-float) stride1 (data2 vector-float) stride2 n)
  (gsl_stats_float_correlation (gsl_vector_float_ptr (entity data1) 0) stride1
                               (gsl_vector_float_ptr (entity data2) 0) stride2 n))

(defgeneric stats-spearman (data1 stride1 data2 stride2 n)
  (:documentation
   "This function computes the Spearman rank correlation coefficient between the
datasets data1 and data2 which must both be of the same length n."))

(defmethod stats-spearman ((data1 vector-double) stride1 (data2 vector-double) stride2 n)
  (cffi:with-foreign-object (work :double (* 2 n))
    (gsl_stats_spearman (gsl_vector_ptr (entity data1) 0) stride1
                        (gsl_vector_ptr (entity data2) 0) stride2 n work)))

(defmethod stats-spearman ((data1 vector-float) stride1 (data2 vector-float) stride2 n)
  (cffi:with-foreign-object (work :float (* 2 n))
    (gsl_stats_float_spearman (gsl_vector_float_ptr (entity data1) 0) stride1
                              (gsl_vector_float_ptr (entity data2) 0) stride2 n work)))

(defgeneric stats-wmean (w wstride data stride n)
  (:documentation
   "This function returns the weighted mean of dataset data with stride and length n,
using the set of weights w with wstride and length n."))

(defmethod stats-wmean ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wmean (gsl_vector_ptr (entity w) 0) wstride
                   (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wmean ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wmean (gsl_vector_float_ptr (entity w) 0) wstride
                         (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wvariance (w wstride data stride n)
  (:documentation
   "This function returns the estimated variance of the dataset data with stride and
length n, using the set of weights w with wstride and length n."))

(defmethod stats-wvariance ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wvariance (gsl_vector_ptr (entity w) 0) wstride
                       (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wvariance ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wvariance (gsl_vector_float_ptr (entity w) 0) wstride
                             (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wvariance-m (w wstride data stride n wmean)
  (:documentation
   "This functions returns the estimated variance of the weighted dataset data using
the given weighted mean wmean."))

(defmethod stats-wvariance-m ((w vector-double) wstride (data vector-double) stride n mean)
  (gsl_stats_wvariance_m (gsl_vector_ptr (entity w) 0) wstride
                         (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-wvariance-m ((w vector-float) wstride (data vector-float) stride n mean)
  (gsl_stats_float_wvariance_m (gsl_vector_float_ptr (entity w) 0) wstride
                               (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-wsd (w wstride data stride n)
  (:documentation
   "The statndard deviation is defined as the square root of the variance. This function
return the square root of the corresponding variance function."))

(defmethod stats-wsd ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wsd (gsl_vector_ptr (entity w) 0) wstride
                 (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wsd ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wsd (gsl_vector_float_ptr (entity w) 0) wstride
                       (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wsd-m (w wstride data stride n wmean)
  (:documentation
   "This function returns the square root of the corresponding variance function
gsl:stats-wvariance-m."))

(defmethod stats-wsd-m ((w vector-double) wstride (data vector-double) stride n wmean)
  (gsl_stats_wsd_m (gsl_vector_ptr (entity w) 0) wstride
                   (gsl_vector_ptr (entity data) 0) stride n wmean))

(defmethod stats-wsd-m ((w vector-float) wstride (data vector-float) stride n wmean)
  (gsl_stats_float_wsd_m (gsl_vector_float_ptr (entity w) 0) wstride
                         (gsl_vector_float_ptr (entity data) 0) stride n wmean))

(defgeneric stats-wvariance-with-fixed-mean (w wstride data stride n mean)
  (:documentation
   "This function computes an unbiased estimate of the variance of the weighted
dataset when the population mean of the underlying distribution is known a
priori. In this case the extimator for the variance replaces the sample mean
by the known population mean."))

(defmethod stats-wvariance-with-fixed-mean ((w vector-double) wstride
                                            (data vector-double) stride n mean)
  (gsl_stats_wvariance_with_fixed_mean (gsl_vector_ptr (entity w) 0) wstride
                                       (gsl_vector_ptr (entity data) 0) stride
                                       n mean))

(defmethod stats-wvariance-with-fixed-mean ((w vector-float) wstride
                                            (data vector-float) stride n mean)
  (gsl_stats_float_wvariance_with_fixed_mean (gsl_vector_float_ptr (entity w) 0) wstride
                                             (gsl_vector_float_ptr (entity data) 0) stride
                                             n mean))

(defgeneric stats-wsd-with-fixed-mean (w wstride data stride n mean)
  (:documentation
   "The standard deviation is defined as the square root of the variance. This function
returns the square root of the correspoding variance function."))

(defmethod stats-wsd-with-fixed-mean ((w vector-double) wstride
                                      (data vector-double) stride n mean)
  (gsl_stats_wsd_with_fixed_mean (gsl_vector_ptr (entity w) 0) wstride
                                 (gsl_vector_ptr (entity data) 0) stride n mean))

(defmethod stats-wsd-with-fixed-mean ((w vector-float) wstride
                                      (data vector-float) stride n mean)
  (gsl_stats_float_wsd_with_fixed_mean (gsl_vector_float_ptr (entity w) 0) wstride
                                       (gsl_vector_float_ptr (entity data) 0) stride n mean))

(defgeneric stats-wtss (w wstride data stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of data about the
weighted mean."))

(defmethod stats-wtss ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wtss (gsl_vector_ptr (entity w) 0) wstride
                  (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wtss ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wtss (gsl_vector_float_ptr (entity w) 0) wstride
                        (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wtss-m (w wstride data stride n wmean)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of data about the
weighted mean."))

(defmethod stats-wtss-m ((w vector-double) wstride (data vector-double) stride n wmean)
  (gsl_stats_wtss_m (gsl_vector_ptr (entity w) 0) wstride
                    (gsl_vector_ptr (entity data) 0) stride n wmean))

(defmethod stats-wtss-m ((w vector-float) wstride (data vector-float) stride n wmean)
  (gsl_stats_float_wtss_m (gsl_vector_float_ptr (entity w) 0) wstride
                          (gsl_vector_float_ptr (entity data) 0) stride n wmean))

(defgeneric stats-wabsdev (w wstride data stride n)
  (:documentation
   "This function computes the weighted absolute deviation from the weighted mean of
data."))

(defmethod stats-wabsdev ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wabsdev (gsl_vector_ptr (entity w) 0) wstride
                     (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wabsdev ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wabsdev (gsl_vector_float_ptr (entity w) 0) wstride
                           (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wabsdev-m (w wstride data stride n wmean)
  (:documentation
   "This function computes the absolute deviation of the weighted dataset data about
the given weighted mean wmean."))

(defmethod stats-wabsdev-m ((w vector-double) wstride (data vector-double) stride n wmean)
  (gsl_stats_wabsdev_m (gsl_vector_ptr (entity w) 0) wstride
                       (gsl_vector_ptr (entity data) 0) stride n wmean))

(defmethod stats-wabsdev-m ((w vector-float) wstride (data vector-float) stride n wmean)
  (gsl_stats_float_wabsdev_m (gsl_vector_float_ptr (entity w) 0) wstride
                             (gsl_vector_float_ptr (entity data) 0) stride n wmean))

(defgeneric stats-wskew (w wstride data stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset data."))

(defmethod stats-wskew ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wskew (gsl_vector_ptr (entity w) 0) wstride
                   (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wskew ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wskew (gsl_vector_float_ptr (entity w) 0) wstride
                         (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wskew-m-sd (w wstride data stride n wmean wsd)
  (:documentation
   "This function computes the weighted skewness of the dataset data using the given
values of the weighted mean and weighted standard deviation, wmean and wsd."))

(defmethod stats-wskew-m-sd ((w vector-double)  wstride
                             (data vector-double) stride
                             n wmean wsd)
  (gsl_stats_wskew_m_sd (gsl_vector_ptr (entity w) 0) wstride
                        (gsl_vector_ptr (entity data) 0) stride
                        n wmean wsd))

(defmethod stats-wskew-m-sd ((w vector-float) wstride
                              (data vector-float) stride
                              n wmean wsd)
  (gsl_stats_float_wskew_m_sd (gsl_vector_float_ptr (entity w) 0) wstride
                              (gsl_vector_float_ptr (entity data) 0) stride
                              n wmean wsd))

(defgeneric stats-wkurtosis (w wstride data stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset data."))

(defmethod stats-wkurtosis ((w vector-double) wstride (data vector-double) stride n)
  (gsl_stats_wkurtosis (gsl_vector_ptr (entity w) 0) wstride
                       (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-wkurtosis ((w vector-float) wstride (data vector-float) stride n)
  (gsl_stats_float_wkurtosis (gsl_vector_float_ptr (entity w) 0) wstride
                             (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-wkurtosis-m-sd (w wstride data stride n wmean wsd)
  (:documentation
   "This function computes the weighted kurtosis of the dataset data using the given
values of the weighted mean and weighted standard deviation, wmean and wsd."))

(defmethod stats-wkurtosis-m-sd ((w vector-double) wstride
                                 (data vector-double) stride
                                 n wmean wsd)
  (gsl_stats_wkurtosis_m_sd (gsl_vector_ptr (entity w) 0) wstride
                            (gsl_vector_ptr (entity data) 0) stride
                            n wmean wsd))

(defmethod stats-wkurtosis-m-sd ((w vector-float) wstride
                                 (data vector-float) stride
                                 n wmean wsd)
  (gsl_stats_float_wkurtosis_m_sd (gsl_vector_float_ptr (entity w) 0) wstride
                                  (gsl_vector_float_ptr (entity data) 0) stride
                                  n wmean wsd))

(defgeneric stats-max (data stride n)
  (:documentation
   "This function returns the maximum value in data, a dataset of length n with stride
The maximum value is defined as the value of the element x_i which satisfies
x_i >= x_j for all j.
If you want instead to find the element with the largest absolute magnitude you will
need to apply abs to your data before calling this function."))

(defmethod stats-max ((data vector-double) stride n)
  (gsl_stats_max (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-max ((data vector-float) stride n)
  (gsl_stats_float_max (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-min (data stride n)
  (:documentation
   "This function returns the minimum value in data, a dataset of length n with stride
The minimum value is defined as the value of the element x_i which satisfies
x_i <= x_j for all j.
If you want istead to find the element with the smallest absolute magnitude you will
need to apply abs to your data before calling this function."))

(defmethod stats-min ((data vector-double) stride n)
  (gsl_stats_min (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-min ((data vector-float) stride n)
  (gsl_stats_float_min (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-minmax (data stride n)
  (:documentation
   "This function finds both the minimum and maximum values min, max in data in a
single pass."))

(defmethod stats-minmax ((data vector-double) stride n)
  (cffi:with-foreign-objects ((min :double) (max :double))
    (gsl_stats_minmax min max (gsl_vector_ptr (entity data) 0) stride n)
    (values (cffi:mem-ref min :double) (cffi:mem-ref max :double))))

(defmethod stats-minmax ((data vector-float) stride n)
  (cffi:with-foreign-objects ((min :float) (max :float))
    (gsl_stats_float_minmax min max (gsl_vector_float_ptr (entity data) 0) stride n)
    (values (cffi:mem-ref min :float) (cffi:mem-ref max :float))))

(defgeneric stats-max-index (data stride n)
  (:documentation
   "This function returns the index of the maximum value in data, a dataset of length
n with stride. The maximum value is defined as the value of the element x_i which
satisfies x_i >= x_j for all j. When there are several equal maximum elements then
the first one is chosen."))

(defmethod stats-max-index ((data vector-double) stride n)
  (gsl_stats_max_index (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-max-index ((data vector-float) stride n)
  (gsl_stats_float_max_index (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-min-index (data stride n)
  (:documentation
   "This function returns the index of the minimum value in data, a dataset of length n
which stride. The minimum value is defined as the value of the element x_i which
satisfies x_i <= x_j for all j. When there are several equal minimum elements then
the first one is chosen."))

(defmethod stats-min-index ((data vector-double) stride n)
  (gsl_stats_min_index (gsl_vector_ptr (entity data) 0) stride n))

(defmethod stats-min-index ((data vector-float) stride n)
  (gsl_stats_float_min_index (gsl_vector_float_ptr (entity data) 0) stride n))

(defgeneric stats-minmax-index (data stride n)
  (:documentation
   "This function returns the indexes min_index, max_index of the minimum and
maximum vlaues in data in a single pass."))

(defmethod stats-minmax-index ((data vector-double) stride n)
  (cffi:with-foreign-objects ((min-index :unsigned-int)
                              (max-index :unsigned-int))
    (gsl_stats_minmax_index min-index max-index
                            (gsl_vector_ptr (entity data) 0) stride n)
    (values (cffi:mem-ref min-index :unsigned-int)
            (cffi:mem-ref max-index :unsigned-int))))

(defmethod stats-minmax-index ((data vector-float) stride n)
  (cffi:with-foreign-objects ((min-index :unsigned-int)
                              (max-index :unsigned-int))
    (gsl_stats_float_minmax_index min-index max-index
                                  (gsl_vector_float_ptr (entity data) 0) stride n)
    (values (cffi:mem-ref min-index :unsigned-int)
            (cffi:mem-ref max-index :unsigned-int))))
