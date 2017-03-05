;;;; cl-scl/gsl/stats.lisp
;;;;
;;;; This file describes the statistical functions in the library. The
;;;; basic statistical functions include routines to compute the mean,
;;;; variance and standard deviation. More advanced functions allow
;;;; you to calculate absolute deviations, skewness, and kurtosis as
;;;; well as the median and arbitrary pecentiles. The algorithms use
;;;; recurrence relations to compute average quantities in a stable
;;;; way, without large intermediate values that might overflow.

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


;;; functions

(defgeneric stats-mean (v &optional stride n)
  (:documentation
   "This function returns the arithmetic mean of data, a dataset of
length n with stride."))

(defmacro anaphoric-stats-vector (&body body)
  `(let ((stride (if (null stride) (scl::stride v) stride))
         (size (if (null n) (scl::size v) n)))
     (cffi:with-pointer-to-vector-data (data (scl::data v))
       ,@body)))

(defmacro make-stats-mean (vtype func)
  `(defmethod stats-mean ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-mean scl::vector-double gsl_stats_mean)

(make-stats-mean scl::vector-float gsl_stats_float_mean)

(defgeneric stats-variance (v &optional stride n)
  (:documentation
   "This function returns the estimated, or sample variance of data, a
dataset of length n with stride."))

(defmacro make-stats-variance (vtype func)
  `(defmethod stats-variance ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-variance scl::vector-double gsl_stats_variance)

(make-stats-variance scl::vector-float gsl_stats_float_variance)

(defgeneric stats-variance-m (v mean &optional stride n)
  (:documentation
   "This function returns the sample variance of data relative to the
given value of mean."))

(defmacro make-stats-variance-m (vtype func)
  `(defmethod stats-variance-m ((v ,vtype) mean &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-variance-m scl::vector-double gsl_stats_variance_m)

(make-stats-variance-m scl::vector-float gsl_stats_float_variance_m)

(defgeneric stats-sd (v &optional stride n)
  (:documentation
   "The standard deviation is defined as the square root of the
variance. This function return the square root of the corresponding
variance function."))

(defmacro make-stats-sd (vtype func)
  `(defmethod stats-sd ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-sd scl::vector-double gsl_stats_sd)

(make-stats-sd scl::vector-float gsl_stats_float_sd)

(defgeneric stats-sd-m (v mean &optional stride n)
  (:documentation
   "The standard deviation is defined as the square root of the
variance. This function return the square root of the corresponding
variance function."))

(defmacro make-stats-sd-m (vtype func)
  `(defmethod stats-sd-m ((v ,vtype) mean &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-sd-m scl::vector-double gsl_stats_sd_m)

(make-stats-sd-m scl::vector-float gsl_stats_float_sd_m)

(defgeneric stats-tss (v &optional stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about
the mean."))

(defmacro make-stats-tss (vtype func)
  `(defmethod stats-tss ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-tss scl::vector-double gsl_stats_tss)

(make-stats-tss scl::vector-float gsl_stats_float_tss)

(defgeneric stats-tss-m (v mean &optional stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about
the mean."))

(defmacro make-stats-tss-m (vtype func)
  `(defmethod stats-tss-m ((v ,vtype) mean &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-tss-m scl::vector-double gsl_stats_tss_m)

(make-stats-tss-m scl::vector-float gsl_stats_float_tss_m)

(defgeneric stats-variance-with-fixed-mean (v mean &optional stride n)
  (:documentation
   "This function computes an unbiased estimate of the variance of
data when the population mean of the underlying distribution is known
a priori. In this case the estimator for the variance uses the factor
1/N."))

(defmacro make-stats-variance-with-fixed-mean (vtype func)
  `(defmethod stats-variance-with-fixed-mean ((v ,vtype) mean
                                              &optional (stride nil) (n nil))
       (anaphoric-stats-vector
         (,func data stride size (coerce mean 'double-float)))))

(make-stats-variance-with-fixed-mean scl::vector-double
                                     gsl_stats_variance_with_fixed_mean)

(make-stats-variance-with-fixed-mean scl::vector-float
                                     gsl_stats_float_variance_with_fixed_mean)

(defgeneric stats-sd-with-fixed-mean (v mean &optional stride n)
  (:documentation
   "This function calculates the standard deviation of data for a
fixed population mean The result is the square root of the
corresponding variance function."))

(defmacro make-stats-sd-with-fixed-mean (vtype func)
  `(defmethod stats-sd-with-fixed-mean ((v ,vtype) mean
                                        &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-sd-with-fixed-mean scl::vector-double
                               gsl_stats_sd_with_fixed_mean)

(make-stats-sd-with-fixed-mean scl::vector-float
                               gsl_stats_float_sd_with_fixed_mean)

(defgeneric stats-absdev (v &optional stride n)
  (:documentation
   "This function computes the absoute deviation from the mean of
data, a dataset of length n with stride."))

(defmacro make-stats-absdev (vtype func)
  `(defmethod stats-absdev ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-absdev scl::vector-double gsl_stats_absdev)

(make-stats-absdev scl::vector-float gsl_stats_float_absdev)

(defgeneric stats-absdev-m (v mean &optional stride n)
  (:documentation
   "This function computes the absolute deviation of the dataset data
relative to the given value of mean. This function is useful if you
have already computed the mean of data (and want to avoid recomputing
it), or wish to calculate the absolute deviation relative to another
value (such as zero, or the median)."))

(defmacro make-stats-absdev-m (vtype func)
  `(defmethod stats-absdev-m ((v ,vtype) mean
                              &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-absdev-m scl::vector-double gsl_stats_absdev_m)

(make-stats-absdev-m scl::vector-float gsl_stats_float_absdev_m)

(defgeneric stats-skew (v &optional stride n)
  (:documentation
   "This function computes the skewness of data, a dataset of length n
with stride. The function computes the mean and estimated standard
deviation of data via calls to gsl_stats_mean and gsl_stats_sd."))

(defmacro make-stats-skew (vtype func)
  `(defmethod stats-skew ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-skew scl::vector-double gsl_stats_skew)

(make-stats-skew scl::vector-float gsl_stats_float_skew)

(defgeneric stats-skew-m-sd (v mean sd &optional stride n)
  (:documentation
   "This function computes the skewness of the dataset data using the
given values of the mean and standard deviation. This function are
usefull if you have already computed the mean and standard deviation
of data and want to avoid recomputing them."))

(defmacro make-stats-skew-m-sd (vtype func)
  `(defmethod stats-skew-m-sd ((v ,vtype) mean sd
                               &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size
              (coerce mean 'double-float)
              (coerce sd 'double-float)))))

(make-stats-skew-m-sd scl::vector-double gsl_stats_skew_m_sd)

(make-stats-skew-m-sd scl::vector-float gsl_stats_float_skew_m_sd)

(defgeneric stats-kurtosis (v &optional stride n)
  (:documentation
   "This function computes the kurtosis of data, a dataset of length n
with stride. The kurtosis measures how sharply peaked a
distribution is, relative to its width.  The kurtosis is normalized
to zero for a Gaussian distribution."))

(defmacro make-stats-kurtosis (vtype func)
  `(defmethod stats-kurtosis ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-kurtosis scl::vector-double gsl_stats_kurtosis)

(make-stats-kurtosis scl::vector-float gsl_stats_float_kurtosis)

(defgeneric stats-kurtosis-m-sd (v mean sd &optional stride n)
  (:documentation
   "This function computes the kurtosis of the dataset data using the
given values of the mean and standard deviation sd. This function is
useful if you have already computed the mean and standard deviation of
data and want to avoid recomputing them."))

(defmacro make-stats-kurtosis-m-sd (vtype func)
  `(defmethod stats-kurtosis-m-sd ((v ,vtype) mean sd
                                   &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size
              (coerce mean 'double-float)
              (coerce sd 'double-float)))))

(make-stats-kurtosis-m-sd scl::vector-double
                          gsl_stats_kurtosis_m_sd)

(make-stats-kurtosis-m-sd scl::vector-float
                          gsl_stats_float_kurtosis_m_sd)

(defgeneric stats-lag1-autocorrelation (v &optional stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset
data."))

(defmacro make-stats-lag1-autocorrelation (vtype func)
  `(defmethod stats-lag1-autocorrelation ((v ,vtype)
                                          &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-lag1-autocorrelation scl::vector-double
                                 gsl_stats_lag1_autocorrelation)

(make-stats-lag1-autocorrelation scl::vector-float
                                 gsl_stats_float_lag1_autocorrelation)

(defgeneric stats-lag1-autocorrelation-m (v mean &optional stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset
data using the given value of the mean."))

(defmacro make-stats-lag1-autocorrelation-m (vtype func)
  `(defmethod stats-lag1-autocorrelation-m ((v ,vtype) mean
                                            &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-lag1-autocorrelation-m scl::vector-double
                                   gsl_stats_lag1_autocorrelation_m)

(make-stats-lag1-autocorrelation-m scl::vector-float
                                   gsl_stats_float_lag1_autocorrelation_m)

(defgeneric stats-covariance (v1 v2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets v1 and v2
which must both be of the same length n."))

(defmacro anaphoric-stats-two-vectors (&body body)
  `(let ((stride1 (if (null stride1) (scl::stride v1) stride1))
         (stride2 (if (null stride2) (scl::stride v2) stride2))
         (size (if (null n) (scl::size v1) n)))
     (cffi:with-pointer-to-vector-data (data1 (scl::data v1))
       (cffi:with-pointer-to-vector-data (data2 (scl::data v2))
         ,@body))))

(defmacro make-stats-covariance (vtype func)
  `(defmethod stats-covariance ((v1 ,vtype) (v2 ,vtype)
                                &optional
                                  (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-vectors
      (,func data1 stride1 data2 stride2 size))))

(make-stats-covariance scl::vector-double
                       gsl_stats_covariance)

(make-stats-covariance scl::vector-float
                       gsl_stats_float_covariance)

(defgeneric stats-covariance-m (v1 v2 mean1 mean2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets v1 and v2
using the given values of the means, mean1 and mean2. This is useful
if you have already computed the mean of v1 and v2 and want to avoid
recomputing them."))

(defmacro make-stats-covariance-m (vtype func)
  `(defmethod stats-covariance-m ((v1 ,vtype) (v2 ,vtype) mean1 mean2
                                  &optional
                                    (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-vectors
       (,func data1 stride1 data2 stride2 size
              (coerce mean1 'double-float)
              (coerce mean2 'double-float)))))

(make-stats-covariance-m scl::vector-double
                         gsl_stats_covariance_m)

(make-stats-covariance-m scl::vector-float
                         gsl_stats_float_covariance_m)

(defgeneric stats-correlation (v1 v2 &optional stride1 stride2 n)
  (:documentation
   "This function efficiently computes the Pearson correlation
coefficient between the datasets v1 and v2 which must both be of the
same length n."))

(defmacro make-stats-correlation (vtype func)
  `(defmethod stats-correlation ((v1 ,vtype) (v2 ,vtype)
                                 &optional
                                   (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-vectors
       (,func data1 stride1 data2 stride2 size))))

(make-stats-correlation scl::vector-double
                        gsl_stats_correlation)

(make-stats-correlation scl::vector-float
                        gsl_stats_float_correlation)

(defgeneric stats-spearman (v1 v2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the Spearman rank correlation coefficient
between the datasets v1 and v2 which must both be of the same length
n."))

(defmacro make-stats-spearman (vtype element-vtype func)
  `(defmethod stats-spearman ((v1 ,vtype) (v2 ,vtype)
                              &optional
                                (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-vectors
      (cffi:with-foreign-object (work ,element-vtype (* 2 size))
        (,func data1 stride1 data2 stride2 size work)))))

(make-stats-spearman scl::vector-double
                     :double gsl_stats_spearman)

(make-stats-spearman scl::vector-float
                     :float gsl_stats_float_spearman)

(defgeneric stats-wmean (w v &optional  wstride stride n)
  (:documentation
   "This function returns the weighted mean of dataset v with stride
and length n, using the set of weights w with wstride and length
n."))

(defmacro anaphoric-stats-two-vectors-with-weight (&body body)
  `(let ((wstride (if (null wstride) (scl::stride w) wstride))
         (stride (if (null stride) (scl::stride v) stride))
         (size (if (null n) (scl::size v) n)))
     (cffi:with-pointer-to-vector-data (w (scl::data w))
       (cffi:with-pointer-to-vector-data (data (scl::data v))
         ,@body))))

(defmacro make-stats-wmean (vtype func)
  `(defmethod stats-wmean ((w ,vtype) (v ,vtype)
                           &optional
                             (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
      (,func w wstride data stride size))))

(make-stats-wmean scl::vector-double gsl_stats_wmean)

(make-stats-wmean scl::vector-float gsl_stats_float_wmean)

(defgeneric stats-wvariance (w v &optional wstride stride n)
  (:documentation
   "This function returns the estimated variance of the dataset v with
stride and length n, using the set of weights w with wstride and
length n."))

(defmacro make-stats-wvariance (vtype func)
  `(defmethod stats-wvariance ((w ,vtype) (v ,vtype)
                               &optional
                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wvariance scl::vector-double gsl_stats_wvariance)

(make-stats-wvariance scl::vector-float gsl_stats_float_wvariance)

(defgeneric stats-wvariance-m (w v wmean &optional wstride stride n)
  (:documentation
   "This functions returns the estimated variance of the weighted
dataset v using the given weighted mean wmean."))

(defmacro make-stats-wvariance-m (vtype func)
  `(defmethod stats-wvariance-m ((w ,vtype) (v ,vtype) wmean
                                 &optional
                                   (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wvariance-m scl::vector-double
                        gsl_stats_wvariance_m)

(make-stats-wvariance-m scl::vector-float
                        gsl_stats_float_wvariance_m)

(defgeneric stats-wsd (w v &optional wstride stride n)
  (:documentation
   "The statndard deviation is defined as the square root of the
variance. This function return the square root of the corresponding
variance function."))

(defmacro make-stats-wsd (vtype func)
  `(defmethod stats-wsd ((w ,vtype) (v ,vtype)
                         &optional
                           (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wsd scl::vector-double gsl_stats_wsd)

(make-stats-wsd scl::vector-float gsl_stats_float_wsd)

(defgeneric stats-wsd-m (w v wmean &optional wstride stride n)
  (:documentation
   "This function returns the square root of the corresponding
variance function gsl:stats-wvariance-m."))

(defmacro make-stats-wsd-m (vtype func)
  `(defmethod stats-wsd-m ((w ,vtype) (v ,vtype) wmean
                           &optional
                             (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wsd-m scl::vector-double gsl_stats_wsd_m)

(make-stats-wsd-m scl::vector-float gsl_stats_float_wsd_m)

(defgeneric stats-wvariance-with-fixed-mean (w v mean
                                             &optional wstride stride n)
  (:documentation
   "This function computes an unbiased estimate of the variance of the
weighted dataset when the population mean of the underlying
distribution is known a priori. In this case the extimator for the
variance replaces the sample mean by the known population mean."))

(defmacro make-stats-wvariance-with-fixed-mean (vtype func)
  `(defmethod stats-wvariance-with-fixed-mean ((w ,vtype) (v ,vtype) mean
                                               &optional
                                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce mean 'double-float)))))

(make-stats-wvariance-with-fixed-mean scl::vector-double
                                      gsl_stats_wvariance_with_fixed_mean)

(make-stats-wvariance-with-fixed-mean scl::vector-float
                                      gsl_stats_float_wvariance_with_fixed_mean)

(defgeneric stats-wsd-with-fixed-mean (w v mean
                                       &optional wstride stride n)
  (:documentation
   "The standard deviation is defined as the square root of the
variance. This function returns the square root of the correspoding
variance function."))

(defmacro make-stats-wsd-with-fixed-mean (vtype func)
  `(defmethod stats-wsd-with-fixed-mean ((w ,vtype) (v ,vtype) mean
                                         &optional
                                           (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce mean 'double-float)))))

(make-stats-wsd-with-fixed-mean scl::vector-double
                                gsl_stats_wsd_with_fixed_mean)

(make-stats-wsd-with-fixed-mean scl::vector-float
                                gsl_stats_float_wsd_with_fixed_mean)

(defgeneric stats-wtss (w v &optional wstride stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of
data about the weighted mean."))

(defmacro make-stats-wtss (vtype func)
  `(defmethod stats-wtss ((w ,vtype) (v ,vtype)
                          &optional
                            (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wtss scl::vector-double gsl_stats_wtss)

(make-stats-wtss scl::vector-float gsl_stats_float_wtss)

(defgeneric stats-wtss-m (w v wmean &optional wstride stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of
data about the weighted mean."))

(defmacro make-stats-wtss-m (vtype func)
  `(defmethod stats-wtss-m ((w ,vtype) (v ,vtype) wmean
                            &optional
                              (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wtss-m scl::vector-double gsl_stats_wtss_m)

(make-stats-wtss-m scl::vector-float gsl_stats_float_wtss_m)

(defgeneric stats-wabsdev (w v &optional wstride stride n)
  (:documentation
   "This function computes the weighted absolute deviation from the
weighted mean of v."))

(defmacro make-stats-wabsdev (vtype func)
  `(defmethod stats-wabsdev ((w ,vtype) (v ,vtype)
                             &optional
                               (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wabsdev scl::vector-double gsl_stats_wabsdev)

(make-stats-wabsdev scl::vector-float gsl_stats_float_wabsdev)

(defgeneric stats-wabsdev-m (w v wmean &optional wstride stride n)
  (:documentation
   "This function computes the absolute deviation of the weighted
dataset v about the given weighted mean wmean."))

(defmacro make-stats-wabsdev-m (vtype func)
  `(defmethod stats-wabsdev-m ((w ,vtype) (v ,vtype) wmean
                               &optional
                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wabsdev-m scl::vector-double
                      gsl_stats_wabsdev_m)

(make-stats-wabsdev-m scl::vector-float
                      gsl_stats_float_wabsdev_m)

(defgeneric stats-wskew (w v &optional wstride stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset v."))

(defmacro make-stats-wskew (vtype func)
  `(defmethod stats-wskew ((w ,vtype) (v ,vtype)
                           &optional
                             (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wskew scl::vector-double gsl_stats_wskew)

(make-stats-wskew scl::vector-float gsl_stats_float_wskew)

(defgeneric stats-wskew-m-sd (w v wmean wsd
                              &optional wstride stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset v
using the given values of the weighted mean and weighted standard
deviation, wmean and wsd."))

(defmacro make-stats-wskew-m-sd (vtype func)
  `(defmethod stats-wskew-m-sd ((w ,vtype) (v ,vtype) wmean wsd
                                &optional
                                  (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)
              (coerce wsd 'double-float)))))

(make-stats-wskew-m-sd scl::vector-double
                       gsl_stats_wskew_m_sd)

(make-stats-wskew-m-sd scl::vector-float
                       gsl_stats_float_wskew_m_sd)

(defgeneric stats-wkurtosis (w v &optional wstride stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset v."))

(defmacro make-stats-wkurtosis (vtype func)
  `(defmethod stats-wkurtosis ((w ,vtype) (v ,vtype)
                               &optional
                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wkurtosis scl::vector-double
                      gsl_stats_wkurtosis)

(make-stats-wkurtosis scl::vector-float
                      gsl_stats_float_wkurtosis)

(defgeneric stats-wkurtosis-m-sd (w v wmean wsd
                                  &optional wstride stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset v
using the given values of the weighted mean and weighted standard
deviation, wmean and wsd."))

(defmacro make-stats-wkurtosis-m-sd (vtype func)
  `(defmethod stats-wkurtosis-m-sd ((w ,vtype) (v ,vtype) wmean wsd
                                    &optional
                                      (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)
              (coerce wsd 'double-float)))))

(make-stats-wkurtosis-m-sd scl::vector-double
                           gsl_stats_wkurtosis_m_sd)

(make-stats-wkurtosis-m-sd scl::vector-float
                           gsl_stats_float_wkurtosis_m_sd)

(defgeneric stats-max (v &optional stride n)
  (:documentation
   "This function returns the maximum value in v, a dataset of length
n with stride The maximum value is defined as the value of the element
x_i which satisfies x_i >= x_j for all j.
If you want instead to find the element with the largest absolute
magnitude you will need to apply abs to your data before calling this
function."))

(defmacro make-stats-max (vtype func)
  `(defmethod stats-max ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-max scl::vector-double gsl_stats_max)

(make-stats-max scl::vector-float gsl_stats_float_max)

(defgeneric stats-min (v &optional stride n)
  (:documentation
   "This function returns the minimum value in v, a dataset of length
n with stride The minimum value is defined as the value of the element
x_i which satisfies x_i <= x_j for all j.
If you want istead to find the element with the smallest absolute
magnitude you will need to apply abs to your data before calling this
function."))

(defmacro make-stats-min (vtype func)
  `(defmethod stats-min ((v ,vtype) &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-min scl::vector-double gsl_stats_min)

(make-stats-min scl::vector-float gsl_stats_float_min)

(defgeneric stats-minmax (v &optional stride n)
  (:documentation
   "This function finds both the minimum and maximum values min, max
in v in a single pass."))

(defmacro make-stats-minmax (vtype element-vtype func)
  `(defmethod stats-minmax ((v ,vtype)
                            &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (cffi:with-foreign-objects ((min ,element-vtype)
                                   (max ,element-vtype))
         (,func min max data stride size)
         (values (cffi:mem-ref min ,element-vtype)
                 (cffi:mem-ref max ,element-vtype))))))

(make-stats-minmax scl::vector-double
                   :double gsl_stats_minmax)

(make-stats-minmax scl::vector-float
                   :float gsl_stats_float_minmax)

(defgeneric stats-max-index (data &optional stride n)
  (:documentation
   "This function returns the index of the maximum value in v, a
dataset of length n with stride. The maximum value is defined as the
value of the element x_i which satisfies x_i >= x_j for all j. When
there are several equal maximum elements then the first one is
chosen."))

(defmacro make-stats-max-index (vtype func)
  `(defmethod stats-max-index ((v ,vtype)
                               &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-max-index scl::vector-double
                      gsl_stats_max_index)

(make-stats-max-index scl::vector-float
                      gsl_stats_float_max_index)

(defgeneric stats-min-index (v &optional stride n)
  (:documentation
   "This function returns the index of the minimum value in v, a
dataset of length n which stride. The minimum value is defined as the
value of the element x_i which satisfies x_i <= x_j for all j. When
there are several equal minimum elements then the first one is
chosen."))

(defmacro make-stats-min-index (vtype func)
  `(defmethod stats-min-index ((v ,vtype)
                               &optional (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-min-index scl::vector-double
                      gsl_stats_min_index)

(make-stats-min-index scl::vector-float
                      gsl_stats_float_min_index)

(defgeneric stats-minmax-index (data &optional stride n)
  (:documentation
   "This function returns the indexes min_index, max_index of the
minimum and maximum vlaues in data in a single pass."))

(defmacro make-stats-minmax-index (vtype func)
  `(defmethod stats-minmax-index ((v ,vtype)
                                  &optional (stride nil) (n nil))
     (anaphoric-stats-vector
       (cffi:with-foreign-objects ((min-index :unsigned-int)
                                   (max-index :unsigned-int))
         (,func min-index max-index data stride size)
         (values (cffi:mem-ref min-index :unsigned-int)
                 (cffi:mem-ref max-index :unsigned-int))))))

(make-stats-minmax-index scl::vector-double
                         gsl_stats_minmax_index)

(make-stats-minmax-index scl::vector-float
                         gsl_stats_float_minmax_index)

(defgeneric stats-median-from-sorted-data (v &optional stride n)
  (:documentation
   "This function returns the median value of sorted v, a dataset of
length n with stride. The elements of the vector must be in ascending
numerical ordar.  There are no checks to see whether the data are
sorted, so the function should always be used first."))

(defmacro make-stats-median-from-sorted-data (vtype func)
  `(defmethod stats-median-from-sorted-data ((v ,vtype)
                                             &optional
                                               (stride nil) (n nil))
     (anaphoric-stats-vector (,func data stride size))))

(make-stats-median-from-sorted-data scl::vector-double
                                    gsl_stats_median_from_sorted_data)

(make-stats-median-from-sorted-data scl::vector-float
                                    gsl_stats_float_median_from_sorted_data)

(defgeneric stats-quantile-from-sorted-data (v f &optional stride n)
  (:documentation
   "This function returns a quantile value of sorted v, a vector of
length n with stride. The elements of the array must be in ascending
numerical order. The quantile is determined by the f, a fraction 0
between 1.  For example, to compute the value of the 75th percentile f
should have the value 0.75."))

(defmacro make-stats-quantile-from-sorted-data (vtype func)
  `(defmethod stats-quantile-from-sorted-data ((v ,vtype) f
                                               &optional
                                                 (stride nil) (n nil))
     (anaphoric-stats-vector
       (let ((point (coerce f 'double-float)))
         (if (not (<= 0.0d0 point 1.0d0))
             (error "a fraction must be between 0 and 1")
             (,func data stride size point))))))

(make-stats-quantile-from-sorted-data scl::vector-double
                                      gsl_stats_quantile_from_sorted_data)

(make-stats-quantile-from-sorted-data scl::vector-float
                                      gsl_stats_float_quantile_from_sorted_data)
