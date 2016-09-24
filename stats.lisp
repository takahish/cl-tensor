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

(defgeneric stats-mean (data &optional stride n)
  (:documentation
   "This function returns the arithmetic mean of data, a dataset of length n with stride."))

(defmacro anaphoric-stats-array (&body body)
  `(let ((st (if (null stride) (stride data) stride))
         (sz (if (null n) (size data) n)))
     (cffi:with-pointer-to-vector-data (ptr (entity data))
       ,@body)))

(defmacro make-stats-mean (class c-func)
  `(defmethod stats-mean ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-mean simple-array-double gsl_stats_mean)

(make-stats-mean simple-array-float gsl_stats_float_mean)

(defgeneric stats-variance (data &optional stride n)
  (:documentation
   "This function returns the estimated, or sample variance of data, a dataset of length
n with stride."))

(defmacro make-stats-variance (class c-func)
  `(defmethod stats-variance ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-variance simple-array-double gsl_stats_variance)

(make-stats-variance simple-array-float gsl_stats_float_variance)

(defgeneric stats-variance-m (data mean &optional stride n)
  (:documentation
   "This function returns the sample variance of data relative to the given value of mean."))

(defmacro make-stats-variance-m (class c-func)
  `(defmethod stats-variance-m ((data ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-variance-m simple-array-double gsl_stats_variance_m)

(make-stats-variance-m simple-array-float gsl_stats_float_variance_m)

(defgeneric stats-sd (data &optional stride n)
  (:documentation
   "The standard deviation is defined as the square root of the variance. This function
return the square root of the corresponding variance function."))

(defmacro make-stats-sd (class c-func)
  `(defmethod stats-sd ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-sd simple-array-double gsl_stats_sd)

(make-stats-sd simple-array-float gsl_stats_float_sd)

(defgeneric stats-sd-m (data mean &optional stride n)
  (:documentation
   "The standard deviation is defined as the square root of the variance. This function
return the square root of the corresponding variance function."))

(defmacro make-stats-sd-m (class c-func)
  `(defmethod stats-sd-m ((data ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-sd-m simple-array-double gsl_stats_sd_m)

(make-stats-sd-m simple-array-float gsl_stats_float_sd_m)

(defgeneric stats-tss (data &optional stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about the mean."))

(defmacro make-stats-tss (class c-func)
  `(defmethod stats-tss ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-tss simple-array-double gsl_stats_tss)

(make-stats-tss simple-array-float gsl_stats_float_tss)

(defgeneric stats-tss-m (data mean &optional stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about the mean."))

(defmacro make-stats-tss-m (class c-func)
  `(defmethod stats-tss-m ((data ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-tss-m simple-array-double gsl_stats_tss_m)

(make-stats-tss-m simple-array-float gsl_stats_float_tss_m)

(defgeneric stats-variance-with-fixed-mean (data mean &optional stride n)
  (:documentation
   "This function computes an unbiased estimate of the variance of data when the
population mean of the underlying distribution is known a priori. In this case
the estimator for the variance uses the factor 1/N."))

(defmacro make-stats-variance-with-fixed-mean (class c-func)
  `(defmethod stats-variance-with-fixed-mean ((data ,class) mean
                                              &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-variance-with-fixed-mean simple-array-double
                                     gsl_stats_variance_with_fixed_mean)

(make-stats-variance-with-fixed-mean simple-array-float
                                     gsl_stats_float_variance_with_fixed_mean)

(defgeneric stats-sd-with-fixed-mean (data mean &optional stride n)
  (:documentation
   "This function calculates the standard deviation of data for a fixed population mean
The result is the square root of the corresponding variance function."))

(defmacro make-stats-sd-with-fixed-mean (class c-func)
  `(defmethod stats-sd-with-fixed-mean ((data ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-sd-with-fixed-mean simple-array-double gsl_stats_sd_with_fixed_mean)

(make-stats-sd-with-fixed-mean simple-array-float gsl_stats_float_sd_with_fixed_mean)

(defgeneric stats-absdev (data &optional stride n)
  (:documentation
   "This function computes the absoute deviation from the mean of data, a dataset of
length n with stride."))

(defmacro make-stats-absdev (class c-func)
  `(defmethod stats-absdev ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-absdev simple-array-double gsl_stats_absdev)

(make-stats-absdev simple-array-float gsl_stats_float_absdev)

(defgeneric stats-absdev-m (data mean &optional stride n)
  (:documentation
   "This function computes the absolute deviation of the dataset data relative to the
given value of mean. This function is useful if you have already computed the mean
of data (and want to avoid recomputing it), or wish to calculate the absolute deviation
relative to another value (such as zero, or the median)."))

(defmacro make-stats-absdev-m (class c-func)
  `(defmethod stats-absdev-m ((data ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-absdev-m simple-array-double gsl_stats_absdev_m)

(make-stats-absdev-m simple-array-float gsl_stats_float_absdev_m)

(defgeneric stats-skew (data &optional stride n)
  (:documentation
   "This function computes the skewness of data, a dataset of length n with stride.
The function computes the mean and estimated standard deviation of data via calls
to gsl_stats_mean and gsl_stats_sd."))

(defmacro make-stats-skew (class c-func)
  `(defmethod stats-skew ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-skew simple-array-double gsl_stats_skew)

(make-stats-skew simple-array-float gsl_stats_float_skew)

(defgeneric stats-skew-m-sd (data mean sd &optional stride n)
  (:documentation
   "This function computes the skewness of the dataset data using the given values
of the mean and standard deviation. This function are usefull if you have already
computed the mean and standard deviation of data and want to avoid recomputing them."))

(defmacro make-stats-skew-m-sd (class c-func)
  `(defmethod stats-skew-m-sd ((data ,class) mean sd &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean sd))))

(make-stats-skew-m-sd simple-array-double gsl_stats_skew_m_sd)

(make-stats-skew-m-sd simple-array-float gsl_stats_float_skew_m_sd)

(defgeneric stats-kurtosis (data &optional stride n)
  (:documentation
   "This function computes the kurtosis of data, a dataset of length n with stride.
The kurtosis measures how sharply peaked a distribution is, relative to its width.
The kurtosis is normalized to zero for a Gaussian distribution."))

(defmacro make-stats-kurtosis (class c-func)
  `(defmethod stats-kurtosis ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-kurtosis simple-array-double gsl_stats_kurtosis)

(make-stats-kurtosis simple-array-float gsl_stats_float_kurtosis)

(defgeneric stats-kurtosis-m-sd (data mean sd &optional stride n)
  (:documentation
   "This function computes the kurtosis of the dataset data using the given values of
the mean and standard deviation sd. This function is useful if you have already
computed the mean and standard deviation of data and want to avoid recomputing them."))

(defmacro make-stats-kurtosis-m-sd (class c-func)
  `(defmethod stats-kurtosis-m-sd ((data ,class) mean sd &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean sd))))

(make-stats-kurtosis-m-sd simple-array-double gsl_stats_kurtosis_m_sd)

(make-stats-kurtosis-m-sd simple-array-float gsl_stats_float_kurtosis_m_sd)

(defgeneric stats-lag1-autocorrelation (data &optional stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset data."))

(defmacro make-stats-lag1-autocorrelation (class c-func)
  `(defmethod stats-lag1-autocorrelation ((data ,class)
                                          &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-lag1-autocorrelation simple-array-double
                                 gsl_stats_lag1_autocorrelation)

(make-stats-lag1-autocorrelation simple-array-float
                                 gsl_stats_float_lag1_autocorrelation)

(defgeneric stats-lag1-autocorrelation-m (data mean &optional stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset data using the
given value of the mean."))

(defmacro make-stats-lag1-autocorrelation-m (class c-func)
  `(defmethod stats-lag1-autocorrelation-m ((data ,class) mean
                                            &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz mean))))

(make-stats-lag1-autocorrelation-m simple-array-double
                                   gsl_stats_lag1_autocorrelation_m)

(make-stats-lag1-autocorrelation-m simple-array-float
                                   gsl_stats_float_lag1_autocorrelation_m)

(defgeneric stats-covariance (data1 data2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets data1 and data2 which must
both be of the same length n."))

(defmacro anaphoric-stats-arrays (&body body)
  `(let ((st1 (if (null stride1) (stride data1) stride1))
         (st2 (if (null stride2) (stride data2) stride2))
         (sz (if (null n) (size data1) n)))
     (cffi:with-pointer-to-vector-data (ptr1 (entity data1))
       (cffi:with-pointer-to-vector-data (ptr2 (entity data2))
         ,@body))))

(defmacro make-stats-covariance (class c-func)
  `(defmethod stats-covariance ((data1 ,class) (data2 ,class)
                                &optional (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-arrays (,c-func ptr1 st1 ptr2 st2 sz))))

(make-stats-covariance simple-array-double gsl_stats_covariance)

(make-stats-covariance simple-array-float gsl_stats_float_covariance)

(defgeneric stats-covariance-m (data1 data2 mean1 mean2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets data1 and data2 using the
given values of the means, mean1 and mean2. This is useful if you have already
computed the mean of data1 and data2 and want to avoid recomputing them."))

(defmacro make-stats-covariance-m (class c-func)
  `(defmethod stats-covariance-m ((data1 ,class) (data2 ,class) mean1 mean2
                                  &optional (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-arrays (,c-func ptr1 st1 ptr2 st2 sz mean1 mean2))))

(make-stats-covariance-m simple-array-double gsl_stats_covariance_m)

(make-stats-covariance-m simple-array-float gsl_stats_float_covariance_m)

(defgeneric stats-correlation (data1 data2 &optional stride1 stride2 n)
  (:documentation
   "This function efficiently computes the Pearson correlation coefficient between the
datasets data1 and data2 which must both be of the same length n."))

(defmacro make-stats-correlation (class c-func)
  `(defmethod stats-correlation ((data1 ,class) (data2 ,class)
                                 &optional (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-arrays (,c-func ptr1 st1 ptr2 st2 sz))))

(make-stats-correlation simple-array-double gsl_stats_correlation)

(make-stats-correlation simple-array-float gsl_stats_float_correlation)

(defgeneric stats-spearman (data1 data2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the Spearman rank correlation coefficient between the
datasets data1 and data2 which must both be of the same length n."))

(defmacro make-stats-spearman (class element-type c-func)
  `(defmethod stats-spearman ((data1 ,class) (data2 ,class)
                              &optional (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-arrays
      (cffi:with-foreign-object (work ,element-type (* 2 sz))
        (,c-func ptr1 st1 ptr2 st2 sz work)))))

(make-stats-spearman simple-array-double :double gsl_stats_spearman)

(make-stats-spearman simple-array-float :float gsl_stats_float_spearman)

(defgeneric stats-wmean (w data &optional  wstride stride n)
  (:documentation
   "This function returns the weighted mean of dataset data with stride and length n,
using the set of weights w with wstride and length n."))

(defmacro anaphoric-stats-array-with-weight (&body body)
  `(let ((wst (if (null wstride) (stride w) wstride))
         (dst (if (null stride) (stride data) stride))
         (sz (if (null n) (size data) n)))
     (cffi:with-pointer-to-vector-data (wptr (entity w))
       (cffi:with-pointer-to-vector-data (dptr (entity data))
         ,@body))))

(defmacro make-stats-wmean (class c-func)
  `(defmethod stats-wmean ((w ,class) (data ,class)
                           &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wmean simple-array-double gsl_stats_wmean)

(make-stats-wmean simple-array-float gsl_stats_float_wmean)

(defgeneric stats-wvariance (w data &optional wstride stride n)
  (:documentation
   "This function returns the estimated variance of the dataset data with stride and
length n, using the set of weights w with wstride and length n."))

(defmacro make-stats-wvariance (class c-func)
  `(defmethod stats-wvariance ((w ,class) (data ,class)
                               &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wvariance simple-array-double gsl_stats_wvariance)

(make-stats-wvariance simple-array-float gsl_stats_float_wvariance)

(defgeneric stats-wvariance-m (w data wmean &optional wstride stride n)
  (:documentation
   "This functions returns the estimated variance of the weighted dataset data using
the given weighted mean wmean."))

(defmacro make-stats-wvariance-m (class c-func)
  `(defmethod stats-wvariance-m ((w ,class) (data ,class) wmean
                                 &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz wmean))))

(make-stats-wvariance-m simple-array-double gsl_stats_wvariance_m)

(make-stats-wvariance-m simple-array-float gsl_stats_float_wvariance_m)

(defgeneric stats-wsd (w data &optional wstride stride n)
  (:documentation
   "The statndard deviation is defined as the square root of the variance. This function
return the square root of the corresponding variance function."))

(defmacro make-stats-wsd (class c-func)
  `(defmethod stats-wsd ((w ,class) (data ,class)
                         &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wsd simple-array-double gsl_stats_wsd)

(make-stats-wsd simple-array-float gsl_stats_float_wsd)

(defgeneric stats-wsd-m (w data wmean &optional wstride stride n)
  (:documentation
   "This function returns the square root of the corresponding variance function
gsl:stats-wvariance-m."))

(defmacro make-stats-wsd-m (class c-func)
  `(defmethod stats-wsd-m ((w ,class) (data ,class) wmean
                           &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz wmean))))

(make-stats-wsd-m simple-array-double gsl_stats_wsd_m)

(make-stats-wsd-m simple-array-float gsl_stats_float_wsd_m)

(defgeneric stats-wvariance-with-fixed-mean (w data mean &optional wstride stride n)
  (:documentation
   "This function computes an unbiased estimate of the variance of the weighted
dataset when the population mean of the underlying distribution is known a
priori. In this case the extimator for the variance replaces the sample mean
by the known population mean."))

(defmacro make-stats-wvariance-with-fixed-mean (class c-func)
  `(defmethod stats-wvariance-with-fixed-mean ((w ,class) (data ,class) mean
                                               &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz mean))))

(make-stats-wvariance-with-fixed-mean simple-array-double
                                      gsl_stats_wvariance_with_fixed_mean)

(make-stats-wvariance-with-fixed-mean simple-array-float
                                      gsl_stats_float_wvariance_with_fixed_mean)

(defgeneric stats-wsd-with-fixed-mean (w data mean &optional wstride stride n)
  (:documentation
   "The standard deviation is defined as the square root of the variance. This function
returns the square root of the correspoding variance function."))

(defmacro make-stats-wsd-with-fixed-mean (class c-func)
  `(defmethod stats-wsd-with-fixed-mean ((w ,class) (data ,class) mean
                                         &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz mean))))

(make-stats-wsd-with-fixed-mean simple-array-double gsl_stats_wsd_with_fixed_mean)

(make-stats-wsd-with-fixed-mean simple-array-float gsl_stats_float_wsd_with_fixed_mean)

(defgeneric stats-wtss (w data &optional wstride stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of data about the
weighted mean."))

(defmacro make-stats-wtss (class c-func)
  `(defmethod stats-wtss ((w ,class) (data ,class)
                          &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wtss simple-array-double gsl_stats_wtss)

(make-stats-wtss simple-array-float gsl_stats_float_wtss)

(defgeneric stats-wtss-m (w data wmean &optional wstride stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of data about the
weighted mean."))

(defmacro make-stats-wtss-m (class c-func)
  `(defmethod stats-wtss-m ((w ,class) (data ,class) wmean
                            &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz wmean))))

(make-stats-wtss-m simple-array-double gsl_stats_wtss_m)

(make-stats-wtss-m simple-array-float gsl_stats_float_wtss_m)

(defgeneric stats-wabsdev (w data &optional wstride stride n)
  (:documentation
   "This function computes the weighted absolute deviation from the weighted mean of
data."))

(defmacro make-stats-wabsdev (class c-func)
  `(defmethod stats-wabsdev ((w ,class) (data ,class)
                             &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wabsdev simple-array-double gsl_stats_wabsdev)

(make-stats-wabsdev simple-array-float gsl_stats_float_wabsdev)

(defgeneric stats-wabsdev-m (w data wmean &optional wstride stride n)
  (:documentation
   "This function computes the absolute deviation of the weighted dataset data about
the given weighted mean wmean."))

(defmacro make-stats-wabsdev-m (class c-func)
  `(defmethod stats-wabsdev-m ((w ,class) (data ,class) wmean
                               &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz wmean))))

(make-stats-wabsdev-m simple-array-double gsl_stats_wabsdev_m)

(make-stats-wabsdev-m simple-array-float gsl_stats_float_wabsdev_m)

(defgeneric stats-wskew (w data &optional wstride stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset data."))

(defmacro make-stats-wskew (class c-func)
  `(defmethod stats-wskew ((w ,class) (data ,class)
                           &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wskew simple-array-double gsl_stats_wskew)

(make-stats-wskew simple-array-float gsl_stats_float_wskew)

(defgeneric stats-wskew-m-sd (w data wmean wsd &optional wstride stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset data using the given
values of the weighted mean and weighted standard deviation, wmean and wsd."))

(defmacro make-stats-wskew-m-sd (class c-func)
  `(defmethod stats-wskew-m-sd ((w ,class) (data ,class) wmean wsd
                                &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz wmean wsd))))

(make-stats-wskew-m-sd simple-array-double gsl_stats_wskew_m_sd)

(make-stats-wskew-m-sd simple-array-float gsl_stats_float_wskew_m_sd)

(defgeneric stats-wkurtosis (w data &optional wstride stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset data."))

(defmacro make-stats-wkurtosis (class c-func)
  `(defmethod stats-wkurtosis ((w ,class) (data ,class)
                               &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz))))

(make-stats-wkurtosis simple-array-double gsl_stats_wkurtosis)

(make-stats-wkurtosis simple-array-float gsl_stats_float_wkurtosis)

(defgeneric stats-wkurtosis-m-sd (w data wmean wsd &optional wstride stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset data using the given
values of the weighted mean and weighted standard deviation, wmean and wsd."))

(defmacro make-stats-wkurtosis-m-sd (class c-func)
  `(defmethod stats-wkurtosis-m-sd ((w ,class) (data ,class) wmean wsd
                                    &optional (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-array-with-weight (,c-func wptr wst dptr dst sz wmean wsd))))

(make-stats-wkurtosis-m-sd simple-array-double gsl_stats_wkurtosis_m_sd)

(make-stats-wkurtosis-m-sd simple-array-float gsl_stats_float_wkurtosis_m_sd)

(defgeneric stats-max (data &optional stride n)
  (:documentation
   "This function returns the maximum value in data, a dataset of length n with stride
The maximum value is defined as the value of the element x_i which satisfies
x_i >= x_j for all j.
If you want instead to find the element with the largest absolute magnitude you will
need to apply abs to your data before calling this function."))

(defmacro make-stats-max (class c-func)
  `(defmethod stats-max ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-max simple-array-double gsl_stats_max)

(make-stats-max simple-array-float gsl_stats_float_max)

(defgeneric stats-min (data &optional stride n)
  (:documentation
   "This function returns the minimum value in data, a dataset of length n with stride
The minimum value is defined as the value of the element x_i which satisfies
x_i <= x_j for all j.
If you want istead to find the element with the smallest absolute magnitude you will
need to apply abs to your data before calling this function."))

(defmacro make-stats-min (class c-func)
  `(defmethod stats-min ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-min simple-array-double gsl_stats_min)

(make-stats-min simple-array-float gsl_stats_float_min)

(defgeneric stats-minmax (data &optional stride n)
  (:documentation
   "This function finds both the minimum and maximum values min, max in data in a
single pass."))

(defmacro make-stats-minmax (class element-type c-func)
  `(defmethod stats-minmax ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array
      (cffi:with-foreign-objects ((min ,element-type) (max ,element-type))
        (,c-func min max ptr st sz)
        (values (cffi:mem-ref min ,element-type) (cffi:mem-ref max ,element-type))))))

(make-stats-minmax simple-array-double :double gsl_stats_minmax)

(make-stats-minmax simple-array-float :float gsl_stats_float_minmax)

(defgeneric stats-max-index (data &optional stride n)
  (:documentation
   "This function returns the index of the maximum value in data, a dataset of length
n with stride. The maximum value is defined as the value of the element x_i which
satisfies x_i >= x_j for all j. When there are several equal maximum elements then
the first one is chosen."))

(defmacro make-stats-max-index (class c-func)
  `(defmethod stats-max-index ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-max-index simple-array-double gsl_stats_max_index)

(make-stats-max-index simple-array-float gsl_stats_float_max_index)

(defgeneric stats-min-index (data &optional stride n)
  (:documentation
   "This function returns the index of the minimum value in data, a dataset of length n
which stride. The minimum value is defined as the value of the element x_i which
satisfies x_i <= x_j for all j. When there are several equal minimum elements then
the first one is chosen."))

(defmacro make-stats-min-index (class c-func)
  `(defmethod stats-min-index ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-min-index simple-array-double gsl_stats_min_index)

(make-stats-min-index simple-array-float gsl_stats_float_min_index)

(defgeneric stats-minmax-index (data &optional stride n)
  (:documentation
   "This function returns the indexes min_index, max_index of the minimum and
maximum vlaues in data in a single pass."))

(defmacro make-stats-minmax-index (class c-func)
  `(defmethod stats-minmax-index ((data ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-array
      (cffi:with-foreign-objects ((min-index :unsigned-int)
                                  (max-index :unsigned-int))
        (,c-func min-index max-index ptr st sz)
        (values (cffi:mem-ref min-index :unsigned-int)
                (cffi:mem-ref max-index :unsigned-int))))))

(make-stats-minmax-index simple-array-double gsl_stats_minmax_index)

(make-stats-minmax-index simple-array-float gsl_stats_float_minmax_index)

(defgeneric stats-median-from-sorted-data (data &optional stride n)
  (:documentation
   "This function returns the median value of sorted data, a dataset of length n
with stride. The elements of the array must be in ascending numerical ordar.
There are no checks to see whether the data are sorted, so the function should
always be used first."))

(defmacro make-stats-median-from-sorted-data (class c-func)
  `(defmethod stats-median-from-sorted-data ((data ,class)
                                             &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz))))

(make-stats-median-from-sorted-data simple-array-double
                                    gsl_stats_median_from_sorted_data)

(make-stats-median-from-sorted-data simple-array-float
                                    gsl_stats_float_median_from_sorted_data)

(defgeneric stats-quantile-from-sorted-data (data f &optional stride n)
  (:documentation
   "This function returns a quantile value of sorted data, a double-precision
array of length n with stride. The elements of the array must be in ascending
numerical order. The quantile is determined by the f, a fraction 0 between 1.
For example, to compute the value of the 75th percentile f should have the
value 0.75."))

(defmacro make-stats-quantile-from-sorted-data (class c-func)
  `(defmethod stats-quantile-from-sorted-data ((data ,class) f
                                               &optional (stride nil) (n nil))
     (anaphoric-stats-array (,c-func ptr st sz f))))

(make-stats-quantile-from-sorted-data simple-array-double
                                      gsl_stats_quantile_from_sorted_data)

(make-stats-quantile-from-sorted-data simple-array-float
                                      gsl_stats_float_quantile_from_sorted_data)
