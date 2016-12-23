;;;; cl-sct/gsl/stats.lisp
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

(defgeneric stats-mean (v &optional stride n)
  (:documentation
   "This function returns the arithmetic mean of data, a dataset of
length n with stride."))

(defmacro anaphoric-stats-simple-vector (&body body)
  `(let ((stride (if (null stride) (sct::stride v) stride))
         (size (if (null n) (sct::size v) n)))
     (cffi:with-pointer-to-vector-data (data (sct::data v))
       ,@body)))

(defmacro make-stats-mean (class func)
  `(defmethod stats-mean ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-mean sct::simple-vector-double gsl_stats_mean)

(make-stats-mean sct::simple-vector-float gsl_stats_float_mean)

(defgeneric stats-variance (v &optional stride n)
  (:documentation
   "This function returns the estimated, or sample variance of data, a
dataset of length n with stride."))

(defmacro make-stats-variance (class func)
  `(defmethod stats-variance ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-variance sct::simple-vector-double gsl_stats_variance)

(make-stats-variance sct::simple-vector-float gsl_stats_float_variance)

(defgeneric stats-variance-m (v mean &optional stride n)
  (:documentation
   "This function returns the sample variance of data relative to the
given value of mean."))

(defmacro make-stats-variance-m (class func)
  `(defmethod stats-variance-m ((v ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-variance-m sct::simple-vector-double gsl_stats_variance_m)

(make-stats-variance-m sct::simple-vector-float gsl_stats_float_variance_m)

(defgeneric stats-sd (v &optional stride n)
  (:documentation
   "The standard deviation is defined as the square root of the
variance. This function return the square root of the corresponding
variance function."))

(defmacro make-stats-sd (class func)
  `(defmethod stats-sd ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-sd sct::simple-vector-double gsl_stats_sd)

(make-stats-sd sct::simple-vector-float gsl_stats_float_sd)

(defgeneric stats-sd-m (v mean &optional stride n)
  (:documentation
   "The standard deviation is defined as the square root of the
variance. This function return the square root of the corresponding
variance function."))

(defmacro make-stats-sd-m (class func)
  `(defmethod stats-sd-m ((v ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-sd-m sct::simple-vector-double gsl_stats_sd_m)

(make-stats-sd-m sct::simple-vector-float gsl_stats_float_sd_m)

(defgeneric stats-tss (v &optional stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about
the mean."))

(defmacro make-stats-tss (class func)
  `(defmethod stats-tss ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-tss sct::simple-vector-double gsl_stats_tss)

(make-stats-tss sct::simple-vector-float gsl_stats_float_tss)

(defgeneric stats-tss-m (v mean &optional stride n)
  (:documentation
   "This function return the total sum of squared (TSS) of data about
the mean."))

(defmacro make-stats-tss-m (class func)
  `(defmethod stats-tss-m ((v ,class) mean &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-tss-m sct::simple-vector-double gsl_stats_tss_m)

(make-stats-tss-m sct::simple-vector-float gsl_stats_float_tss_m)

(defgeneric stats-variance-with-fixed-mean (v mean &optional stride n)
  (:documentation
   "This function computes an unbiased estimate of the variance of
data when the population mean of the underlying distribution is known
a priori. In this case the estimator for the variance uses the factor
1/N."))

(defmacro make-stats-variance-with-fixed-mean (class func)
  `(defmethod stats-variance-with-fixed-mean ((v ,class) mean
                                              &optional (stride nil) (n nil))
       (anaphoric-stats-simple-vector
         (,func data stride size (coerce mean 'double-float)))))

(make-stats-variance-with-fixed-mean sct::simple-vector-double
                                     gsl_stats_variance_with_fixed_mean)

(make-stats-variance-with-fixed-mean sct::simple-vector-float
                                     gsl_stats_float_variance_with_fixed_mean)

(defgeneric stats-sd-with-fixed-mean (v mean &optional stride n)
  (:documentation
   "This function calculates the standard deviation of data for a
fixed population mean The result is the square root of the
corresponding variance function."))

(defmacro make-stats-sd-with-fixed-mean (class func)
  `(defmethod stats-sd-with-fixed-mean ((v ,class) mean
                                        &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-sd-with-fixed-mean sct::simple-vector-double
                               gsl_stats_sd_with_fixed_mean)

(make-stats-sd-with-fixed-mean sct::simple-vector-float
                               gsl_stats_float_sd_with_fixed_mean)

(defgeneric stats-absdev (v &optional stride n)
  (:documentation
   "This function computes the absoute deviation from the mean of
data, a dataset of length n with stride."))

(defmacro make-stats-absdev (class func)
  `(defmethod stats-absdev ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-absdev sct::simple-vector-double gsl_stats_absdev)

(make-stats-absdev sct::simple-vector-float gsl_stats_float_absdev)

(defgeneric stats-absdev-m (v mean &optional stride n)
  (:documentation
   "This function computes the absolute deviation of the dataset data
relative to the given value of mean. This function is useful if you
have already computed the mean of data (and want to avoid recomputing
it), or wish to calculate the absolute deviation relative to another
value (such as zero, or the median)."))

(defmacro make-stats-absdev-m (class func)
  `(defmethod stats-absdev-m ((v ,class) mean
                              &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-absdev-m sct::simple-vector-double gsl_stats_absdev_m)

(make-stats-absdev-m sct::simple-vector-float gsl_stats_float_absdev_m)

(defgeneric stats-skew (v &optional stride n)
  (:documentation
   "This function computes the skewness of data, a dataset of length n
with stride. The function computes the mean and estimated standard
deviation of data via calls to gsl_stats_mean and gsl_stats_sd."))

(defmacro make-stats-skew (class func)
  `(defmethod stats-skew ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-skew sct::simple-vector-double gsl_stats_skew)

(make-stats-skew sct::simple-vector-float gsl_stats_float_skew)

(defgeneric stats-skew-m-sd (v mean sd &optional stride n)
  (:documentation
   "This function computes the skewness of the dataset data using the
given values of the mean and standard deviation. This function are
usefull if you have already computed the mean and standard deviation
of data and want to avoid recomputing them."))

(defmacro make-stats-skew-m-sd (class func)
  `(defmethod stats-skew-m-sd ((v ,class) mean sd
                               &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size
              (coerce mean 'double-float)
              (coerce sd 'double-float)))))

(make-stats-skew-m-sd sct::simple-vector-double gsl_stats_skew_m_sd)

(make-stats-skew-m-sd sct::simple-vector-float gsl_stats_float_skew_m_sd)

(defgeneric stats-kurtosis (v &optional stride n)
  (:documentation
   "This function computes the kurtosis of data, a dataset of length n
with stride. The kurtosis measures how sharply peaked a
distribution is, relative to its width.  The kurtosis is normalized
to zero for a Gaussian distribution."))

(defmacro make-stats-kurtosis (class func)
  `(defmethod stats-kurtosis ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-kurtosis sct::simple-vector-double gsl_stats_kurtosis)

(make-stats-kurtosis sct::simple-vector-float gsl_stats_float_kurtosis)

(defgeneric stats-kurtosis-m-sd (v mean sd &optional stride n)
  (:documentation
   "This function computes the kurtosis of the dataset data using the
given values of the mean and standard deviation sd. This function is
useful if you have already computed the mean and standard deviation of
data and want to avoid recomputing them."))

(defmacro make-stats-kurtosis-m-sd (class func)
  `(defmethod stats-kurtosis-m-sd ((v ,class) mean sd
                                   &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size
              (coerce mean 'double-float)
              (coerce sd 'double-float)))))

(make-stats-kurtosis-m-sd sct::simple-vector-double
                          gsl_stats_kurtosis_m_sd)

(make-stats-kurtosis-m-sd sct::simple-vector-float
                          gsl_stats_float_kurtosis_m_sd)

(defgeneric stats-lag1-autocorrelation (v &optional stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset
data."))

(defmacro make-stats-lag1-autocorrelation (class func)
  `(defmethod stats-lag1-autocorrelation ((v ,class)
                                          &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-lag1-autocorrelation sct::simple-vector-double
                                 gsl_stats_lag1_autocorrelation)

(make-stats-lag1-autocorrelation sct::simple-vector-float
                                 gsl_stats_float_lag1_autocorrelation)

(defgeneric stats-lag1-autocorrelation-m (v mean &optional stride n)
  (:documentation
   "This function computes the lag-1 autocorrelation of the dataset
data using the given value of the mean."))

(defmacro make-stats-lag1-autocorrelation-m (class func)
  `(defmethod stats-lag1-autocorrelation-m ((v ,class) mean
                                            &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (,func data stride size (coerce mean 'double-float)))))

(make-stats-lag1-autocorrelation-m sct::simple-vector-double
                                   gsl_stats_lag1_autocorrelation_m)

(make-stats-lag1-autocorrelation-m sct::simple-vector-float
                                   gsl_stats_float_lag1_autocorrelation_m)

(defgeneric stats-covariance (v1 v2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets v1 and v2
which must both be of the same length n."))

(defmacro anaphoric-stats-two-simple-vectors (&body body)
  `(let ((stride1 (if (null stride1) (sct::stride v1) stride1))
         (stride2 (if (null stride2) (sct::stride v2) stride2))
         (size (if (null n) (sct::size v1) n)))
     (cffi:with-pointer-to-vector-data (data1 (sct::data v1))
       (cffi:with-pointer-to-vector-data (data2 (sct::data v2))
         ,@body))))

(defmacro make-stats-covariance (class func)
  `(defmethod stats-covariance ((v1 ,class) (v2 ,class)
                                &optional
                                  (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-simple-vectors
      (,func data1 stride1 data2 stride2 size))))

(make-stats-covariance sct::simple-vector-double
                       gsl_stats_covariance)

(make-stats-covariance sct::simple-vector-float
                       gsl_stats_float_covariance)

(defgeneric stats-covariance-m (v1 v2 mean1 mean2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the covariance of the datasets v1 and v2
using the given values of the means, mean1 and mean2. This is useful
if you have already computed the mean of v1 and v2 and want to avoid
recomputing them."))

(defmacro make-stats-covariance-m (class func)
  `(defmethod stats-covariance-m ((v1 ,class) (v2 ,class) mean1 mean2
                                  &optional
                                    (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-simple-vectors
       (,func data1 stride1 data2 stride2 size
              (coerce mean1 'double-float)
              (coerce mean2 'double-float)))))

(make-stats-covariance-m sct::simple-vector-double
                         gsl_stats_covariance_m)

(make-stats-covariance-m sct::simple-vector-float
                         gsl_stats_float_covariance_m)

(defgeneric stats-correlation (v1 v2 &optional stride1 stride2 n)
  (:documentation
   "This function efficiently computes the Pearson correlation
coefficient between the datasets v1 and v2 which must both be of the
same length n."))

(defmacro make-stats-correlation (class func)
  `(defmethod stats-correlation ((v1 ,class) (v2 ,class)
                                 &optional
                                   (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-simple-vectors
       (,func data1 stride1 data2 stride2 size))))

(make-stats-correlation sct::simple-vector-double
                        gsl_stats_correlation)

(make-stats-correlation sct::simple-vector-float
                        gsl_stats_float_correlation)

(defgeneric stats-spearman (v1 v2 &optional stride1 stride2 n)
  (:documentation
   "This function computes the Spearman rank correlation coefficient
between the datasets v1 and v2 which must both be of the same length
n."))

(defmacro make-stats-spearman (class element-type func)
  `(defmethod stats-spearman ((v1 ,class) (v2 ,class)
                              &optional
                                (stride1 nil) (stride2 nil) (n nil))
     (anaphoric-stats-two-simple-vectors
      (cffi:with-foreign-object (work ,element-type (* 2 size))
        (,func data1 stride1 data2 stride2 size work)))))

(make-stats-spearman sct::simple-vector-double
                     :double gsl_stats_spearman)

(make-stats-spearman sct::simple-vector-float
                     :float gsl_stats_float_spearman)

(defgeneric stats-wmean (w v &optional  wstride stride n)
  (:documentation
   "This function returns the weighted mean of dataset v with stride
and length n, using the set of weights w with wstride and length
n."))

(defmacro anaphoric-stats-two-simple-vectors-with-weight (&body body)
  `(let ((wstride (if (null wstride) (sct::stride w) wstride))
         (stride (if (null stride) (sct::stride v) stride))
         (size (if (null n) (sct::size v) n)))
     (cffi:with-pointer-to-vector-data (w (sct::data w))
       (cffi:with-pointer-to-vector-data (data (sct::data v))
         ,@body))))

(defmacro make-stats-wmean (class func)
  `(defmethod stats-wmean ((w ,class) (v ,class)
                           &optional
                             (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
      (,func w wstride data stride size))))

(make-stats-wmean sct::simple-vector-double gsl_stats_wmean)

(make-stats-wmean sct::simple-vector-float gsl_stats_float_wmean)

(defgeneric stats-wvariance (w v &optional wstride stride n)
  (:documentation
   "This function returns the estimated variance of the dataset v with
stride and length n, using the set of weights w with wstride and
length n."))

(defmacro make-stats-wvariance (class func)
  `(defmethod stats-wvariance ((w ,class) (v ,class)
                               &optional
                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wvariance sct::simple-vector-double gsl_stats_wvariance)

(make-stats-wvariance sct::simple-vector-float gsl_stats_float_wvariance)

(defgeneric stats-wvariance-m (w v wmean &optional wstride stride n)
  (:documentation
   "This functions returns the estimated variance of the weighted
dataset v using the given weighted mean wmean."))

(defmacro make-stats-wvariance-m (class func)
  `(defmethod stats-wvariance-m ((w ,class) (v ,class) wmean
                                 &optional
                                   (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wvariance-m sct::simple-vector-double
                        gsl_stats_wvariance_m)

(make-stats-wvariance-m sct::simple-vector-float
                        gsl_stats_float_wvariance_m)

(defgeneric stats-wsd (w v &optional wstride stride n)
  (:documentation
   "The statndard deviation is defined as the square root of the
variance. This function return the square root of the corresponding
variance function."))

(defmacro make-stats-wsd (class func)
  `(defmethod stats-wsd ((w ,class) (v ,class)
                         &optional
                           (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wsd sct::simple-vector-double gsl_stats_wsd)

(make-stats-wsd sct::simple-vector-float gsl_stats_float_wsd)

(defgeneric stats-wsd-m (w v wmean &optional wstride stride n)
  (:documentation
   "This function returns the square root of the corresponding
variance function gsl:stats-wvariance-m."))

(defmacro make-stats-wsd-m (class func)
  `(defmethod stats-wsd-m ((w ,class) (v ,class) wmean
                           &optional
                             (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wsd-m sct::simple-vector-double gsl_stats_wsd_m)

(make-stats-wsd-m sct::simple-vector-float gsl_stats_float_wsd_m)

(defgeneric stats-wvariance-with-fixed-mean (w v mean
                                             &optional wstride stride n)
  (:documentation
   "This function computes an unbiased estimate of the variance of the
weighted dataset when the population mean of the underlying
distribution is known a priori. In this case the extimator for the
variance replaces the sample mean by the known population mean."))

(defmacro make-stats-wvariance-with-fixed-mean (class func)
  `(defmethod stats-wvariance-with-fixed-mean ((w ,class) (v ,class) mean
                                               &optional
                                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce mean 'double-float)))))

(make-stats-wvariance-with-fixed-mean sct::simple-vector-double
                                      gsl_stats_wvariance_with_fixed_mean)

(make-stats-wvariance-with-fixed-mean sct::simple-vector-float
                                      gsl_stats_float_wvariance_with_fixed_mean)

(defgeneric stats-wsd-with-fixed-mean (w v mean
                                       &optional wstride stride n)
  (:documentation
   "The standard deviation is defined as the square root of the
variance. This function returns the square root of the correspoding
variance function."))

(defmacro make-stats-wsd-with-fixed-mean (class func)
  `(defmethod stats-wsd-with-fixed-mean ((w ,class) (v ,class) mean
                                         &optional
                                           (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce mean 'double-float)))))

(make-stats-wsd-with-fixed-mean sct::simple-vector-double
                                gsl_stats_wsd_with_fixed_mean)

(make-stats-wsd-with-fixed-mean sct::simple-vector-float
                                gsl_stats_float_wsd_with_fixed_mean)

(defgeneric stats-wtss (w v &optional wstride stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of
data about the weighted mean."))

(defmacro make-stats-wtss (class func)
  `(defmethod stats-wtss ((w ,class) (v ,class)
                          &optional
                            (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wtss sct::simple-vector-double gsl_stats_wtss)

(make-stats-wtss sct::simple-vector-float gsl_stats_float_wtss)

(defgeneric stats-wtss-m (w v wmean &optional wstride stride n)
  (:documentation
   "This function return the weighted total sum of squares (TSS) of
data about the weighted mean."))

(defmacro make-stats-wtss-m (class func)
  `(defmethod stats-wtss-m ((w ,class) (v ,class) wmean
                            &optional
                              (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wtss-m sct::simple-vector-double gsl_stats_wtss_m)

(make-stats-wtss-m sct::simple-vector-float gsl_stats_float_wtss_m)

(defgeneric stats-wabsdev (w v &optional wstride stride n)
  (:documentation
   "This function computes the weighted absolute deviation from the
weighted mean of v."))

(defmacro make-stats-wabsdev (class func)
  `(defmethod stats-wabsdev ((w ,class) (v ,class)
                             &optional
                               (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wabsdev sct::simple-vector-double gsl_stats_wabsdev)

(make-stats-wabsdev sct::simple-vector-float gsl_stats_float_wabsdev)

(defgeneric stats-wabsdev-m (w v wmean &optional wstride stride n)
  (:documentation
   "This function computes the absolute deviation of the weighted
dataset v about the given weighted mean wmean."))

(defmacro make-stats-wabsdev-m (class func)
  `(defmethod stats-wabsdev-m ((w ,class) (v ,class) wmean
                               &optional
                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)))))

(make-stats-wabsdev-m sct::simple-vector-double
                      gsl_stats_wabsdev_m)

(make-stats-wabsdev-m sct::simple-vector-float
                      gsl_stats_float_wabsdev_m)

(defgeneric stats-wskew (w v &optional wstride stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset v."))

(defmacro make-stats-wskew (class func)
  `(defmethod stats-wskew ((w ,class) (v ,class)
                           &optional
                             (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wskew sct::simple-vector-double gsl_stats_wskew)

(make-stats-wskew sct::simple-vector-float gsl_stats_float_wskew)

(defgeneric stats-wskew-m-sd (w v wmean wsd
                              &optional wstride stride n)
  (:documentation
   "This function computes the weighted skewness of the dataset v
using the given values of the weighted mean and weighted standard
deviation, wmean and wsd."))

(defmacro make-stats-wskew-m-sd (class func)
  `(defmethod stats-wskew-m-sd ((w ,class) (v ,class) wmean wsd
                                &optional
                                  (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)
              (coerce wsd 'double-float)))))

(make-stats-wskew-m-sd sct::simple-vector-double
                       gsl_stats_wskew_m_sd)

(make-stats-wskew-m-sd sct::simple-vector-float
                       gsl_stats_float_wskew_m_sd)

(defgeneric stats-wkurtosis (w v &optional wstride stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset v."))

(defmacro make-stats-wkurtosis (class func)
  `(defmethod stats-wkurtosis ((w ,class) (v ,class)
                               &optional
                                 (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size))))

(make-stats-wkurtosis sct::simple-vector-double
                      gsl_stats_wkurtosis)

(make-stats-wkurtosis sct::simple-vector-float
                      gsl_stats_float_wkurtosis)

(defgeneric stats-wkurtosis-m-sd (w v wmean wsd
                                  &optional wstride stride n)
  (:documentation
   "This function computes the weighted kurtosis of the dataset v
using the given values of the weighted mean and weighted standard
deviation, wmean and wsd."))

(defmacro make-stats-wkurtosis-m-sd (class func)
  `(defmethod stats-wkurtosis-m-sd ((w ,class) (v ,class) wmean wsd
                                    &optional
                                      (wstride nil) (stride nil) (n nil))
     (anaphoric-stats-two-simple-vectors-with-weight
       (,func w wstride data stride size
              (coerce wmean 'double-float)
              (coerce wsd 'double-float)))))

(make-stats-wkurtosis-m-sd sct::simple-vector-double
                           gsl_stats_wkurtosis_m_sd)

(make-stats-wkurtosis-m-sd sct::simple-vector-float
                           gsl_stats_float_wkurtosis_m_sd)

(defgeneric stats-max (v &optional stride n)
  (:documentation
   "This function returns the maximum value in v, a dataset of length
n with stride The maximum value is defined as the value of the element
x_i which satisfies x_i >= x_j for all j.
If you want instead to find the element with the largest absolute
magnitude you will need to apply abs to your data before calling this
function."))

(defmacro make-stats-max (class func)
  `(defmethod stats-max ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-max sct::simple-vector-double gsl_stats_max)

(make-stats-max sct::simple-vector-float gsl_stats_float_max)

(defgeneric stats-min (v &optional stride n)
  (:documentation
   "This function returns the minimum value in v, a dataset of length
n with stride The minimum value is defined as the value of the element
x_i which satisfies x_i <= x_j for all j.
If you want istead to find the element with the smallest absolute
magnitude you will need to apply abs to your data before calling this
function."))

(defmacro make-stats-min (class func)
  `(defmethod stats-min ((v ,class) &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-min sct::simple-vector-double gsl_stats_min)

(make-stats-min sct::simple-vector-float gsl_stats_float_min)

(defgeneric stats-minmax (v &optional stride n)
  (:documentation
   "This function finds both the minimum and maximum values min, max
in v in a single pass."))

(defmacro make-stats-minmax (class element-type func)
  `(defmethod stats-minmax ((v ,class)
                            &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (cffi:with-foreign-objects ((min ,element-type)
                                   (max ,element-type))
         (,func min max data stride size)
         (values (cffi:mem-ref min ,element-type)
                 (cffi:mem-ref max ,element-type))))))

(make-stats-minmax sct::simple-vector-double
                   :double gsl_stats_minmax)

(make-stats-minmax sct::simple-vector-float
                   :float gsl_stats_float_minmax)

(defgeneric stats-max-index (data &optional stride n)
  (:documentation
   "This function returns the index of the maximum value in v, a
dataset of length n with stride. The maximum value is defined as the
value of the element x_i which satisfies x_i >= x_j for all j. When
there are several equal maximum elements then the first one is
chosen."))

(defmacro make-stats-max-index (class func)
  `(defmethod stats-max-index ((v ,class)
                               &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-max-index sct::simple-vector-double
                      gsl_stats_max_index)

(make-stats-max-index sct::simple-vector-float
                      gsl_stats_float_max_index)

(defgeneric stats-min-index (v &optional stride n)
  (:documentation
   "This function returns the index of the minimum value in v, a
dataset of length n which stride. The minimum value is defined as the
value of the element x_i which satisfies x_i <= x_j for all j. When
there are several equal minimum elements then the first one is
chosen."))

(defmacro make-stats-min-index (class func)
  `(defmethod stats-min-index ((v ,class)
                               &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-min-index sct::simple-vector-double
                      gsl_stats_min_index)

(make-stats-min-index sct::simple-vector-float
                      gsl_stats_float_min_index)

(defgeneric stats-minmax-index (data &optional stride n)
  (:documentation
   "This function returns the indexes min_index, max_index of the
minimum and maximum vlaues in data in a single pass."))

(defmacro make-stats-minmax-index (class func)
  `(defmethod stats-minmax-index ((v ,class)
                                  &optional (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (cffi:with-foreign-objects ((min-index :unsigned-int)
                                   (max-index :unsigned-int))
         (,func min-index max-index data stride size)
         (values (cffi:mem-ref min-index :unsigned-int)
                 (cffi:mem-ref max-index :unsigned-int))))))

(make-stats-minmax-index sct::simple-vector-double
                         gsl_stats_minmax_index)

(make-stats-minmax-index sct::simple-vector-float
                         gsl_stats_float_minmax_index)

(defgeneric stats-median-from-sorted-data (v &optional stride n)
  (:documentation
   "This function returns the median value of sorted v, a dataset of
length n with stride. The elements of the vector must be in ascending
numerical ordar.  There are no checks to see whether the data are
sorted, so the function should always be used first."))

(defmacro make-stats-median-from-sorted-data (class func)
  `(defmethod stats-median-from-sorted-data ((v ,class)
                                             &optional
                                               (stride nil) (n nil))
     (anaphoric-stats-simple-vector (,func data stride size))))

(make-stats-median-from-sorted-data sct::simple-vector-double
                                    gsl_stats_median_from_sorted_data)

(make-stats-median-from-sorted-data sct::simple-vector-float
                                    gsl_stats_float_median_from_sorted_data)

(defgeneric stats-quantile-from-sorted-data (v f &optional stride n)
  (:documentation
   "This function returns a quantile value of sorted v, a vector of
length n with stride. The elements of the array must be in ascending
numerical order. The quantile is determined by the f, a fraction 0
between 1.  For example, to compute the value of the 75th percentile f
should have the value 0.75."))

(defmacro make-stats-quantile-from-sorted-data (class func)
  `(defmethod stats-quantile-from-sorted-data ((v ,class) f
                                               &optional
                                                 (stride nil) (n nil))
     (anaphoric-stats-simple-vector
       (let ((point (coerce f 'double-float)))
         (if (not (<= 0.0d0 point 1.0d0))
             (error "a fraction must be between 0 and 1")
             (,func data stride size point))))))

(make-stats-quantile-from-sorted-data sct::simple-vector-double
                                      gsl_stats_quantile_from_sorted_data)

(make-stats-quantile-from-sorted-data sct::simple-vector-float
                                      gsl_stats_float_quantile_from_sorted_data)
