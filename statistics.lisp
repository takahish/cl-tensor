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
;;;; along with this program. If not, see http://www.gnu.org/licenses/.

(cl:defpackage "SB-GSL-STATS"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL"
        "SB-SYS")
  (:export "GSL-STATS-MEAN"
           "GSL-STATS-VARIANCE"
           "GSL-STATS-VARIANCE-M"
           "GSL-STATS-SD"
           "GSL-STATS-SD-M"
           "GSL-STATS-TSS"
           "GSL-STATS-TSS-M"
           "GSL-STATS-VARIANCE-WITH-FIXED-MEAN"
           "GSL-STATS-SD-WITH-FIXED-MEAN"
           "STATS-MEAN"
           "STATS-VARIANCE"
           "STATS-VARIANCE-M"
           "STATS-SD"
           "STATS-SD-M"
           "STATS-TSS"
           "STATS-TSS-M"
           "STATS-VARIANCE-WITH-FIXED-MEAN"
           "STATS-SD-WITH-FIXED-MEAN"))

(cl:in-package "SB-GSL-STATS")

;;; (gsl-stats-mean data stride n)
;;;   This function retruns the arithmetic mean of data, a dataset of length n with stride
;;;   stride.
(define-alien-routine gsl-stats-mean
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t))

;;; (gsl-stats-variance data stride n)
;;;   This function retruns the estimated, or sample, variance of data, a dataset of length
;;;   n with stride stride.
(define-alien-routine gsl-stats-variance
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t))

;;; (gsl-stats-variance-m data stride n)
;;;   This function retruns the sample variance of data relative to the given value of mean.
(define-alien-routine gsl-stats-variance-m
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-sd data stride n)
;;; (gsl-stats-sd-m data stride n mean)
;;;   The standard deviation is defined as the square root of the variance. These functions
;;;   retrun the square root of the corresponding variance function above.
(define-alien-routine gsl-stats-sd
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t))

(define-alien-routine gsl-stats-sd-m
    double
  (data (* double)) ; using pointer type insted of (array double nil)
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
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t))

(define-alien-routine gsl-stats-tss-m
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-variance-with-fixed-mean data stride n mean)
;;;   This function computes an unbiased estimate of the variance of data when the pop-
;;;   ulation mean mean of the underlying distribution is known apriori.
(define-alien-routine gsl-stats-variance-with-fixed-mean
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t)
  (mean double))

;;; (gsl-stats-sd-with-fixed-mean data stride n mean)
;;;   This function calculates the standard deviation of data for a fixed population mean
;;;   mean.
(define-alien-routine gsl-stats-sd-with-fixed-mean
    double
  (data (* double)) ; using pointer type insted of (array double nil)
  (stride size-t)
  (n size-t)
  (mean double))

(defun stats-mean (data)
  "This function retruns the arithmetic mean of data. A type of data must be
'(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-mean (vector-sap data) 1 (length data))))

(defun stats-variance (data)
  "This function retruns the estimated, or sample, variance of data. A type of data
must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-variance (vector-sap data) 1 (length data))))

(defun stats-variance-m (data mean)
  "This function retruns the sample variance of data relative to the given value of mean.
A type of data must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-variance-m (vector-sap data) 1 (length data) (coerce mean 'double-float))))

(defun stats-sd (data)
  "The standard deviation is defined as the square root of the variance. A type of data
must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-sd (vector-sap data) 1 (length data))))

(defun stats-sd-m (data mean)
  "The standard deviation is defined as the square root of the variance. A type of data
must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-sd-m (vector-sap data) 1 (length data) (coerce mean 'double-float))))

(defun stats-tss (data)
  "This function returns the total sum of sqares (TSS) of data about mean. A type of data
must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-tss (vector-sap data) 1 (length data))))

(defun stats-tss-m (data mean)
  "This function returns the total sum of sqares (TSS) of data about mean. A type of data
must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-tss-m (vector-sap data) 1 (length data) (coerce mean 'double-float))))

(defun stats-variance-with-fixed-mean (data mean)
  "This function computes an unbiased estimate of the variance of data when the pop-
ulation mean mean of the underlying distribution is known apriori. A type of data
must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-variance-with-fixed-mean (vector-sap data)
                                        1
                                        (length data)
                                        (coerce mean 'double-float))))

(defun stats-sd-with-fixed-mean (data mean)
  "This function calculates the standard deviation of data for a fixed population mean
mean. A type of data must be '(simple-array double-float ({data length}))."
  (with-pinned-objects (data)
    (gsl-stats-sd-with-fixed-mean (vector-sap data)
                                  1
                                  (length data)
                                  (coerce mean 'double-float))))

;;; (test-gsl-stats)
;;;   This function do tests.
(defun test-gsl-stats ()
  (let ((vec (make-array 100 :initial-element 0.0d0 :element-type 'double-float)))
    (dotimes (i 100)
      (setf (aref vec i) (coerce i 'double-float)))
    (format t "STATS-MEAN: ~A~%" (stats-mean vec))
    (format t "STATS-VARIANCE: ~A~%" (stats-variance vec))
    (format t "STATS-VARIANCE-M: ~A~%" (stats-variance-m vec 50.d0))
    (format t "STATS-SD: ~A~%" (stats-sd vec))
    (format t "STATS-SD-M: ~A~%" (stats-sd-m vec 50.0d0))
    (format t "STATS-TSS: ~A~%" (stats-tss vec))
    (format t "STATS-TSS-M: ~A~%" (stats-tss-m vec 50.d0))
    (format t "STATS-VARIANCE-WITH-FIXED-MEAN: ~A~%"
            (stats-variance-with-fixed-mean vec 50.0d0))
    (format t "STATS-SD-WITH-FIXED-MEAN: ~A~%"
            (stats-sd-with-fixed-mean vec 50.0d0))))
