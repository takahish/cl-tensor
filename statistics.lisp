;;;; sbcl-gsl/statistics.lisp
;;;;
;;;; This file discribes the statistical functions in the library. The basic
;;;; statistical functions include routines to compute the mean, variance and
;;;; standard deviation. More advanced function allow you to calculate absolute
;;;; deviation, skewness, and kurtosis as well as the median and arbitrary
;;;; percentiles.

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
;;;; along with gsl-examples. If not, see http://www.gnu.org/licenses/.

(cl:defpackage "SB-GSL"
  (:use "CL" "SB-ALIEN" "SB-C-CALL")
  (:export 'gsl-stats-mean
           'gsl-stats-variance
           'gsl-stats-variance-m
           'gsl-stats-sd
           'gsl-stats-sd-m
           'gsl-stats-tss
           'gsl-stats-tss-m
           'gsl-stats-variance-with-fixed-mean
           'gsl-stats-sd-with-fixed-mean)))

(cl:in-package "SB-GSL")

;;; (gsl-stats-mean data stride n)
;;;   This function retruns the arithmetic mean of data, a dataset of length n with stride
;;;   stride.
(define-alien-routine gsl-stats-mean
    double
  (data (array double nil))
  (stride size-t)
  (n size-t))

;;; (gsl-stats-variance data stride n)
;;;   This function retruns the estimated, or sample, variance of data, a dataset of length
;;;   n with stride stride.
(define-alien-routine gsl-stats-variance
    double
  (data (array double nil))
  (stride size-t)
  (n size-t))

;;; (gsl-stats-variance-m data stride n)
;;;   This function retruns the sample variance of data relative to the given value of mean.
(define-alien-routine gsl-stats-variance-m
    double
  (data (array double nil))
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-sd data stride n)
;;; (gsl-stats-sd-m data stride n mean)
;;;   The standard deviation is defined as the square root of the variance. These functions
;;;   retrun the square root of the corresponding variance function above.
(define-alien-routine gsl-stats-sd
    double
  (data (array double nil))
  (stride size-t)
  (n size-t))

(define-alien-routine gsl-stats-sd-m
    double
  (data (array double nil))
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-tss data stride n)
;;; (gsl-stats-tss-m data stride n mean)
;;;   Theas functions returns the total sum of sqares (TSS) of data about mean. For
;;;   gsl-stats-tss-m the user-supplied value of mean is used, and for gsl-stats-tss it
;;;   is computed using gsl-stats-mean.
(define-alien-routine gsl-stats-tss
    double
  (data (array double nil))
  (stride size-t)
  (n size-t))

(define-alien-routine gsl-stats-tss-m
    double
  (data (array double nil))
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-variance-with-fixed-mean data stride n mean)
;;;   This function computes an unbiased estimate of the variance of data when the pop-
;;;   ulation mean mean of the underlying distribution is known apriori.
(define-alien-routine gsl-stats-variance-with-fixed-mean
    double
  (data (array double nil))
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-sd-with-fixed-mean data stride n mean)
;;;   This function calculates the standard deviation of data for a fixed population mean
;;;   mean.
(define-alien-routine gsl-stats-sd-with-fixed-mean
    double
  (data (array double nil))
  (stride size-t)
  (n size-t)
  (mean double))

;;; (test-gsl-stats)
;;;   This function do tests.
(defun test-gsl-stats ()
  (with-alien ((c-array (array double 100)))
    (dotimes (i 100)
      (setf (deref c-array i) (coerce i 'double-float)))
    (format t "GSL-STATS-MEAN: ~A~%" (gsl-stats-mean c-array 1 100))
    (format t "GSL-STATS-VARIANCE: ~A~%" (gsl-stats-variance c-array 1 100))
    (format t "GSL-STATS-VARIANCE-M: ~A~%" (gsl-stats-variance-m c-array 1 100 50.0d0))
    (format t "GSL-STATS-SD: ~A~%" (gsl-stats-sd c-array 1 100))
    (format t "GSL-STATS-SD-M: ~A~%" (gsl-stats-sd-m c-array 1 100 50.0d0))
    (format t "GSL-STATS-TSS: ~A~%" (gsl-stats-tss c-array 1 100))
    (format t "GSL-STATS-TSS-M: ~A~%" (gsl-stats-tss-m c-array 1 100 50.0d0))
    (format t "GSL-STATS-VARIANCE-WITH-FIXED-MEAN: ~A~%"
            (gsl-stats-variance-with-fixed-mean c-array 1 100 50.0d0))
    (format t "GSL-STATS-SD-WITH-FIXED-MEAN: ~A~%"
            (gsl-stats-sd-with-fixed-mean c-array 1 100 50.0d0))))
