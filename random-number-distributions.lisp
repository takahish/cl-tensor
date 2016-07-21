;;;; sbcl-gsl/random-number-distributions.lisp
;;;;
;;;; This file describes for generating random variates and computing their
;;;; probability distributions. Samples from the distributions described in this
;;;; file can be obtained using any of the random number generators in the library
;;;; as an underlying source of randomness.

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

(cl:defpackage "GSL-RAN"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL"
        "GSL-RNG")
  (:export "GSL-RAN-GAUSSIAN"
           "GSL-RAN-GAUSSIAN-PDF"
           "GSL-RAN-GAUSSIAN-ZIGGURAT"
           "GSL-RAN-GAUSSIAN-RATIO-METHOD"
           "GSL-RAN-UGAUSSIAN"
           "GSL-RAN-UGAUSSIAN-PDF"
           "GSL-RAN-UGAUSSIAN-RATIO-METHOD"
           "GSL-CDF-GAUSSIAN-P"
           "GSL-CDF-GAUSSIAN-Q"
           "GSL-CDF-GAUSSIAN-PINV"
           "GSL-CDF-GAUSSIAN-QINV"
           "GSL-CDF-UGAUSSIAN-P"
           "GSL-CDF-UGAUSSIAN-Q"
           "GSL-CDF-UGAUSSIAN-PINV"
           "GSL-CDF-UGAUSSIAN-QINV"
           "GSL-RAN-GAUSSIAN-TAIL"
           "GSL-RAN-GAUSSIAN-TAIL-PDF"
           "GSL-RAN-UGAUSSIAN-TAIL"
           "GSL-RAN-UGAUSSIAN-TAIL-PDF"
           "GSL-RAN-BIVARIATE-GAUSSIAN"
           "GSL-RAN-BIVARIATE-GAUSSIAN-PDF"
           "GSL-RAN-EXPONENTIAL"
           "GSL-RAN-EXPONENTIAL-PDF"
           "GSL-CDF-EXPONENTIAL-P"
           "GSL-CDF-EXPONENTIAL-Q"
           "GSL-CDF-EXPONENTIAL-PINV"
           "GSL-CDF-EXPONENTIAL-QINV"
           "GSL-RAN-LAPLACE"
           "GSL-RAN-LAPLACE-PDF"
           "GSL-CDF-LAPLACE-P"
           "GSL-CDF-LAPLACE-Q"
           "GSL-CDF-LAPLACE-PINV"
           "GSL-CDF-LAPLACE-QINV"
           "GSL-RAN-CHISQ"
           "GSL-RAN-CHISQ-PDF"
           "GSL-CDF-CHISQ-P"
           "GSL-CDF-CHISQ-Q"
           "GSL-CDF-CHISQ-PINV"
           "GSL-CDF-CHISQ-QINV"
           "GSL-RAN-FDIST"
           "GSL-RAN-FDIST-PDF"
           "GSL-CDF-FDIST-P"
           "GSL-CDF-FDIST-Q"
           "GSL-CDF-FDIST-PINV"
           "GSL-CDF-FDIST-QINV"
           "GSL-RAN-TDIST"
           "GSL-RAN-TDIST-PDF"
           "GSL-CDF-TDIST-P"
           "GSL-CDF-TDIST-Q"
           "GSL-CDF-TDIST-PINV"
           "GSL-CDF-TDIST-QINV"
           "GSL-RAN-DIR-2D"
           "GSL-RAN-DIR-2D-TRIG-METHOD"
           "GSL-RAN-POISSON"
           "GSL-RAN-POISSON-PDF"
           "GSL-CDF-POISSON-P"
           "GSL-CDF-POISSON-Q"
           "GSL-RAN-BERNOULLI"
           "GSL-RAN-BERNOULLI-PDF"
           "GSL-RAN-BINOMIAL"
           "GSL-RAN-BINOMIAL-PDF"
           "GSL-CDF-BINOMIAL-P"
           "GSL-CDF-BINOMIAL-Q"
           "RAN-GAUSSIAN"
           "RAN-GAUSSIAN-PDF"
           "RAN-GAUSSIAN-ZIGGURAT"
           "RAN-GAUSSIAN-RATIO-METHOD"
           "RAN-UGAUSSIAN"
           "RAN-UGAUSSIAN-PDF"
           "RAN-UGAUSSIAN-RATIO-METHOD"
           "CDF-GAUSSIAN-P"
           "CDF-GAUSSIAN-Q"
           "CDF-GAUSSIAN-PINV"
           "CDF-GAUSSIAN-QINV"
           "CDF-UGAUSSIAN-P"
           "CDF-UGAUSSIAN-Q"
           "CDF-UGAUSSIAN-PINV"
           "CDF-UGAUSSIAN-QINV"
           "RAN-GAUSSIAN-TAIL"
           "RAN-GAUSSIAN-TAIL-PDF"
           "RAN-UGAUSSIAN-TAIL"
           "RAN-UGAUSSIAN-TAIL-PDF"
           "RAN-BIVARIATE-GAUSSIAN"
           "RAN-BIVARIATE-GAUSSIAN-PDF"
           "RAN-EXPONENTIAL"
           "RAN-EXPONENTIAL-PDF"
           "CDF-EXPONENTIAL-P"
           "CDF-EXPONENTIAL-Q"
           "CDF-EXPONENTIAL-PINV"
           "CDF-EXPONENTIAL-QINV"
           "RAN-LAPLACE"
           "RAN-LAPLACE-PDF"
           "CDF-LAPLACE-P"
           "CDF-LAPLACE-Q"
           "CDF-LAPLACE-PINV"
           "CDF-LAPLACE-QINV"
           "RAN-CHISQ"
           "RAN-CHISQ-PDF"
           "CDF-CHISQ-P"
           "CDF-CHISQ-Q"
           "CDF-CHISQ-PINV"
           "CDF-CHISQ-QINV"
           "RAN-FDIST"
           "RAN-FDIST-PDF"
           "CDF-FDIST-P"
           "CDF-FDIST-Q"
           "CDF-FDIST-PINV"
           "CDF-FDIST-QINV"
           "RAN-TDIST"
           "RAN-TDIST-PDF"
           "CDF-TDIST-P"
           "CDF-TDIST-Q"
           "CDF-TDIST-PINV"
           "CDF-TDIST-QINV"
           "RAN-DIR-2D"
           "RAN-DIR-2D-TRIG-METHOD"
           "RAN-POISSON"
           "RAN-POISSON-PDF"
           "CDF-POISSON-P"
           "CDF-POISSON-Q"
           "RAN-BERNOULLI"
           "RAN-BERNOULLI-PDF"
           "RAN-BINOMIAL"
           "RAN-BINOMIAL-PDF"
           "CDF-BINOMIAL-P"
           "CDF-BINOMIAL-Q"))

(cl:in-package "GSL-RAN")

;;; The Gaussian Distribution

;;; (gsl-ran-gaussian rng sigma)
;;;   This function returns a Gaussian random variate, with mean zero and standard
;;;   deviation sigma. Use the transformation z = mu + x on the numbers returned
;;;   by gsl-ran-gaussian to obtain a Gaussian distribution with mean mu. This
;;;   function uses the Box-Muller algorithm which requires two calls to the random
;;;   number generator r.
(define-alien-routine gsl-ran-gaussian
    double
  (rng (* (struct gsl-rng)))
  (sigma double))

;;; (gsl-ran-gaussian-pdf x sigma)
;;;   This function computes the probability density p(x) at x for a Gaussian distribution
;;;   with standard deviation sigma.
(define-alien-routine gsl-ran-gaussian-pdf
    double
  (x double)
  (sigma double))

;;; (gsl-ran-gaussian-ziggurat rng sigma)
;;; (gsl-ran-gaussian-ratio-method rng sigma)
;;;   This function computes a Gaussian random variate using the alternative Marsaglia-
;;;   Tsang ziggurat and Kinderman-Monahoan-Leva ratio methods. The Ziggurat algorithm
;;;   is the fastest available algorithm in most cases.
(define-alien-routine gsl-ran-gaussian-ziggurat
    double
  (rng (* (struct gsl-rng)))
  (sigma double))

(define-alien-routine gsl-ran-gaussian-ratio-method
    double
  (rng (* (struct gsl-rng)))
  (sigma double))

;;; (gsl-ran-ugaussian r)
;;; (gsl-ran-ugaussian-pdf x)
;;; (gsl-ran-ugaussian-ratio-method r)
;;;   These functions compute results for the unit Gaussian distribution. They are equivalent
;;;   to the functions above with a standard deviation of one, sigma = 1.
(define-alien-routine gsl-ran-ugaussian
    double
  (rng (* (struct gsl-rng))))

(define-alien-routine gsl-ran-ugaussian-pdf
    double
  (x double))

(define-alien-routine gsl-ran-ugaussian-ratio-method
    double
  (rng (* (struct gsl-rng))))

;;; (gsl-cdf-gaussian-p x sigma)
;;; (gsl-cdf-gaussian-q x sigma)
;;; (gsl-cdf-gaussian-pinv p sigma)
;;; (gsl-cdf-gaussian-qinv q sigma)
;;;   These functions compute the cumulative distribution functions P(x), Q(x) and their
;;;   inverses for the Gaussian distribution with standard deviation sigma.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-gaussian-p for c-function "gsl_cdf_gaussian_P".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-gaussian-p))
(defun gsl-cdf-gaussian-p (x sigma)
  (with-alien ((gsl-cdf-gaussian-p (function double double double)
                                   :extern "gsl_cdf_gaussian_P"))
    (values (alien-funcall gsl-cdf-gaussian-p x sigma))))

;; function gsl-cdf-gaussian-q for c-function "gsl_cdf_gaussian_Q".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-gaussian-q))
(defun gsl-cdf-gaussian-q (x sigma)
  (with-alien ((gsl-cdf-gaussian-q (function double double double)
                                   :extern "gsl_cdf_gaussian_Q"))
    (values (alien-funcall gsl-cdf-gaussian-q x sigma))))

;; function gsl-cdf-gaussian-pinv for c-function "gsl_cdf_gaussian_Pinv.
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-gaussian-pinv))
(defun gsl-cdf-gaussian-pinv (p sigma)
  (with-alien ((gsl-cdf-gaussian-pinv (function double double double)
                                      :extern "gsl_cdf_gaussian_Pinv"))
    (values (alien-funcall gsl-cdf-gaussian-pinv p sigma))))

;; function gsl-cdf-gaussian-qinv for c-function "gsl_cdf_gaussian_Qinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-gaussian-qinv))
(defun gsl-cdf-gaussian-qinv (q sigma)
  (with-alien ((gsl-cdf-gaussian-qinv (function double double double)
                                      :extern "gsl_cdf_gaussian_Qinv"))
    (values (alien-funcall gsl-cdf-gaussian-qinv q sigma))))

;;; (gsl-cdf-ugaussian-p x)
;;; (gsl-cdf-ugaussian-q x)
;;; (gsl-cdf-ugaussian-pinv x)
;;; (gsl-cdf-ugaussian-qinv x)
;;;   These functions compute the cumulative distribution functions P(x), Q(x) and their
;;;   inverses for the unit Gaussian distribution.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-ugaussian-p for c-function "gsl_cdf_ugaussian_P".
(declaim (ftype (function (t) (values (alien double) &optional)) gsl-cdf-ugaussian-p))
(defun gsl-cdf-ugaussian-p (x)
  (with-alien ((gsl-cdf-ugaussian-p (function double double)
                                    :extern "gsl_cdf_ugaussian_P"))
    (values (alien-funcall gsl-cdf-ugaussian-p x))))

;; function gsl-cdf-ugaussian-q for c-function "gsl_cdf_ugaussian_Q".
(declaim (ftype (function (t) (values (alien double) &optional)) gsl-cdf-ugaussian-q))
(defun gsl-cdf-ugaussian-q (x)
  (with-alien ((gsl-cdf-ugaussian-q (function double double)
                                    :extern "gsl_cdf_ugaussian_Q"))
    (values (alien-funcall gsl-cdf-ugaussian-q x))))

;; function gsl-cdf-ugaussian-pinv for c-function "gsl_cdf_ugaussian_Pinv".
(declaim (ftype (function (t) (values (alien double) &optional)) gsl-cdf-ugaussian-pinv))
(defun gsl-cdf-ugaussian-pinv (p)
  (with-alien ((gsl-cdf-ugaussian-pinv (function double double)
                                       :extern "gsl_cdf_ugaussian_Pinv"))
    (values (alien-funcall gsl-cdf-ugaussian-pinv p))))

;; function gsl-cdf-ugaussian-qinv for c-function "gsl_cdf_ugaussian_Qinv".
(declaim (ftype (function (t) (values (alien double) &optional)) gsl-cdf-ugaussian-qinv))
(defun gsl-cdf-ugaussian-qinv (q)
  (with-alien ((gsl-cdf-ugaussian-qinv (function double double)
                                       :extern "gsl_cdf_ugaussian_Qinv"))
    (values (alien-funcall gsl-cdf-ugaussian-qinv q))))

;;; The Gaussian Tail Distribution

;;; (gsl-ran-gaussian-tail rng a sigma)
;;;   This function provides random variates from the upper tail of a Gaussian distribution
;;;   with standard deviation sigma. The values returned are lagger than the lower limit a,
;;;   which must be positive. The method is based on Marsaglia's famous rectangle-wedge-tail
;;;   algorithm.
(define-alien-routine gsl-ran-gaussian-tail
    double
  (rng (* (struct gsl-rng)))
  (a double)
  (sigma double))

;;; (gsl-ran-gaussian-tail-pdf x a sigma)
;;;   This function computes the probability density p(x) at x for Gaussian tail distribution
;;;   with standard deviation sigma and lower limit a.
(define-alien-routine gsl-ran-gaussian-tail-pdf
    double
  (x double)
  (a double)
  (sigma double))

;;; (gsl-ran-ugaussian-tail rng double a)
;;; (gsl-ran-ugaussian-tail-pdf x a)
;;;   These functions compute result for the tail of a unit Gaussian distribution. They
;;;   are equivalent to the functions above with a standard deviation of one, sigma = 1.
(define-alien-routine gsl-ran-ugaussian-tail
    double
  (rng (* (struct gsl-rng)))
  (a double))

(define-alien-routine gsl-ran-ugaussian-tail-pdf
    double
  (x double)
  (a double))

;;; The Bivariate Gaussian Distribution

;;; (gsl-ran-bivariate-gaussian rng sigma-x sigma-y rho x y)
;;;   This function generates a pair of correlated Gaussian variates, with mean zero,
;;;   correlation coefficient rho and standard deviations sigma-x and sigma-y in the x
;;;   and y directions.
(define-alien-routine gsl-ran-bivariate-gaussian
    void
  (rng (* (struct gsl-rng)))
  (sigma-x double)
  (sigma-y double)
  (rho double)
  (x (* double))
  (y (* double)))

;;; (gsl-ran-bivariate-gaussian-pdf x y sigma-x sigma-y rho)
;;;   This function computes the probability density p(x, y) at (x, y) for bivariate
;;;   Gaussian distribution with standard deviations sigma-x, sigma-y and correlation
;;;   coefficient rho.
(define-alien-routine gsl-ran-bivariate-gaussian-pdf
    double
  (x double)
  (y double)
  (sigma-x double)
  (sigma-y double)
  (rho double))

;;; The Exponential Distribution

;;; (gsl-ran-exponential rng mu)
;;;   This function returns a random variate from the exponential distribution  with mean mu.
(define-alien-routine gsl-ran-exponential
    double
  (rng (* (struct gsl-rng)))
  (mu double))

;;; (gsl-ran-exponential-pdf x mu)
;;;   This function computes the probability density p(x) at x for an exponential distribution
;;;   with mean mu.
(define-alien-routine gsl-ran-exponential-pdf
    double
  (x double)
  (mu double))

;;; (gsl-cdf-exponential-p x mu)
;;; (gsl-cdf-exponential-q x mu)
;;; (gsl-cdf-exponential-pinv p mu)
;;; (gsl-cdf-exponential-qinv q mu)
;;;   These functions compute the cumulative distribution functions P(x), Q(x) and their
;;;   inverses for the exponential distribution with mean mu.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-exponential-p for c-function "gsl_cdf_exponential_P".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-exponential-p))
(defun gsl-cdf-exponential-p (x mu)
  (with-alien ((gsl-cdf-exponential-p (function double double double)
                                      :EXTERN "gsl_cdf_exponential_P"))
    (values (alien-funcall gsl-cdf-exponential-p x mu))))

;; function gsl-cdf-exponential-q for c-function "gsl_cdf_exponential_Q".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-exponential-q))
(defun gsl-cdf-exponential-q (x mu)
  (with-alien ((gsl-cdf-exponential-q (function double double double)
                                      :extern "gsl_cdf_exponential_Q"))
    (values (alien-funcall gsl-cdf-exponential-q x mu))))

;; function gsl-cdf-exponential-pinv for c-function "gsl_cdf_exponential_Pinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-exponential-pinv))
(defun gsl-cdf-exponential-pinv (p mu)
  (with-alien ((gsl-cdf-exponential-pinv (function double double double)
                                         :extern "gsl_cdf_exponential_Pinv"))
    (values (alien-funcall gsl-cdf-exponential-pinv p mu))))

;; function gsl-cdf-exponential-qinv for c-function "gsl_cdf_exponential_Qinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-exponential-qinv))
(defun gsl-cdf-exponential-qinv (q mu)
  (with-alien ((gsl-cdf-exponential-qinv (function double double double)
                                         :extern "gsl_cdf_exponential_Qinv"))
    (values (alien-funcall gsl-cdf-exponential-qinv q mu))))

;;; The Laplace Distribution

;;; (gsl-ran-laplace rng a)
;;;   This function returns a random variate from the Laplace distribution with width a.
(define-alien-routine gsl-ran-laplace
    double
  (rng (* (struct gsl-rng)))
  (a double))

;;; (gsl-ran-laplace-pdf x a)
;;;   This function computes the probability density p(x) at x for a Laplace distribution
;;;   with width a.
(define-alien-routine gsl-ran-laplace-pdf
    double
  (x double)
  (a double))

;;; (gsl-cdf-laplace-p x a)
;;; (gsl-cdf-laplace-q x a)
;;; (gsl-cdf-laplace-pinv p a)
;;; (gsl-cdf-laplace-qinv q a)
;;;   These functions compute the cumulative distribution function P(x), Q(x) and their
;;;   inverses for the Laplace distribution with width a.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-laplace-p for c-function "gsl_cdf_laplace_P".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-laplace-p))
(defun gsl-cdf-laplace-p (x a)
  (with-alien ((gsl-cdf-laplace-p (function double double double)
                                  :extern "gsl_cdf_laplace_P"))
    (values (alien-funcall gsl-cdf-laplace-p x a))))

;; function gsl-cdf-laplace-q for c-function "gsl_cdf_laplace_Q".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-laplace-q))
(defun gsl-cdf-laplace-q (x a)
  (with-alien ((gsl-cdf-laplace-q (function double double double)
                                  :extern "gsl_cdf_laplace_Q"))
    (values (alien-funcall gsl-cdf-laplace-q x a))))

;; function gsl-cdf-laplace-pinv for c-function "gsl_cdf_laplace_Pinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-laplace-pinv))
(defun gsl-cdf-laplace-pinv (p a)
  (with-alien ((gsl-cdf-laplace-pinv (function double double double)
                                     :extern "gsl_cdf_laplace_Pinv"))
    (values (alien-funcall gsl-cdf-laplace-pinv p a))))

;; function gsl-cdf-laplace-qinv for c-function "gsl_cdf_laplace_Qinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-laplace-qinv))
(defun gsl-cdf-laplace-qinv (q a)
  (with-alien ((gsl-cdf-laplace-qinv (function double double double)
                                     :extern "gsl_cdf_laplace_Qinv"))
    (values (alien-funcall gsl-cdf-laplace-qinv q a))))

;;; The chi-squared distribution

;;; (gsl-ran-chisq rng nu)
;;;   This function returns a random variate from the chi-squared distribution with nu
;;;   degrees of freedom.
(define-alien-routine gsl-ran-chisq
    double
  (rng (* (struct gsl-rng)))
  (nu double))

;;; (gsl-ran-chisq-pdf x nu)
;;;   This function computes the probability density p(x) at x for a chi-squared distribution
;;;   with nu degrees of freedom.
(define-alien-routine gsl-ran-chisq-pdf
    double
  (x double)
  (nu double))

;;; (gsl-cdf-chisq-p x nu)
;;; (gsl-cdf-chisq-q x nu)
;;; (gsl-cdf-chisq-pinv p nu)
;;; (gsl-cdf-chisq-qinv q nu)
;;;   These functions compute the cumulative distribution functions P(x), Q(x) and their
;;;   inverses for the chi-squared distribution with nu degrees of freedom.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-chisq-p for c-function "gsl_cdf_chisq_P".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-chisq-p))
(defun gsl-cdf-chisq-p (x nu)
  (with-alien ((gsl-cdf-chisq-p (function double double double)
                                :extern "gsl_cdf_chisq_P"))
    (values (alien-funcall gsl-cdf-chisq-p x nu))))

;; function gsl-cdf-chisq-q for c-function "gsl_cdf_chisq_Q".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-chisq-q))
(defun gsl-cdf-chisq-q (x nu)
  (with-alien ((gsl-cdf-chisq-q (function double double double)
                                :extern "gsl_cdf_chisq_Q"))
    (values (alien-funcall gsl-cdf-chisq-q x nu))))

;; function gsl-cdf-chisq-pinv for c-function "gsl_cdf_chisq_Pinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-chisq-pinv))
(defun gsl-cdf-chisq-pinv (p nu)
  (with-alien ((gsl-cdf-chisq-pinv (function double double double)
                                   :extern "gsl_cdf_chisq_Pinv"))
    (values (alien-funcall gsl-cdf-chisq-pinv p nu))))

;; function gsl-cdf-chisq-qinv for c-function "gsl_cdf_chisq_Qinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-chisq-qinv))
(defun gsl-cdf-chisq-qinv (q nu)
  (with-alien ((gsl-cdf-chisq-qinv (function double double double)
                                   :extern "gsl_cdf_chisq_Qinv"))
    (values (alien-funcall gsl-cdf-chisq-qinv q nu))))

;;; The F-distribution

;;; (gsl-ran-fdist rng nu1 nu2)
;;;   This function returns a random variate from the F-distribution with degrees of
;;;   freedom nu1 and nu2.o
(define-alien-routine gsl-ran-fdist
    double
  (rng (* (struct gsl-rng)))
  (nu1 double)
  (nu2 double))

;;; (gsl-ran-fdist-pdf x nu1 nu2)
;;;   This function computes the probability density p(x) at x for an F-distribution with
;;;   nu1 and nu2 degrees of freedom.
(define-alien-routine gsl-ran-fdist-pdf
    double
  (x double)
  (nu1 double)
  (nu2 double))

;;; (gsl-cdf-fdist-p x nu1 nu2)
;;; (gsl-cdf-fdist-q x nu1 nu2)
;;; (gsl-cdf-fdist-pinv p nu1 nu2)
;;; (gsl-cdf-fdist-qinv q nu1 nu2)
;;;   These functions compute the cumulative distribution functions P(x), Q(x) and their
;;;   inverses for the F-distribution with nu1 and nu2 degrees of freedom.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-fdist-p for c-function "gsl_cdf_dist_P".
(declaim (ftype (function (t t t) (values (alien double) &optional)) gsl-cdf-fdist-p))
(defun gsl-cdf-fdist-p (x nu1 nu2)
  (with-alien ((gsl-cdf-fdist-p (function double double double double)
                                :extern "gsl_cdf_fdist_P"))
    (values (alien-funcall gsl-cdf-fdist-p x nu1 nu2))))

;; function gsl-cdf-fdist-q for c-function "gsl_cdf_fdist_Q".
(declaim (ftype (function (t t t) (values (alien double) &optional)) gsl-cdf-fdist-q))
(defun gsl-cdf-fdist-q (x nu1 nu2)
  (with-alien ((gsl-cdf-fdist-q (function double double double double)
                                :EXTERN "gsl_cdf_fdist_Q"))
    (values (alien-funcall gsl-cdf-fdist-q x nu1 nu2))))

;; function gsl-cdf-fdist-pinv for c-function "gsl_cdf_dist_Pinv".
(declaim (ftype (function (t t t) (values (alien double) &optional)) gsl-cdf-fdist-pinv))
(defun gsl-cdf-fdist-pinv (p nu1 nu2)
  (with-alien ((gsl-cdf-fdist-pinv (function double double double double)
                                   :extern "gsl_cdf_fdist_Pinv"))
    (values (alien-funcall gsl-cdf-fdist-pinv p nu1 nu2))))

;; function gsl-cdf-fdist-qinv for c-function "gsl_cdf_dist_qinv".
(declaim (ftype (function (t t t) (values (alien double) &optional)) gsl-cdf-fdist-qinv))
(defun gsl-cdf-fdist-qinv (q nu1 nu2)
  (with-alien ((gsl-cdf-fdist-qinv (function double double double double)
                                   :extern "gsl_cdf_fdist_Qinv"))
    (values (alien-funcall gsl-cdf-fdist-qinv q nu1 nu2))))

;;; The t-distribution

;;; (gsl-ran-tdist rng nu)
;;;   This function returns a random variate from the t-distribution.
(define-alien-routine gsl-ran-tdist
    double
  (rng (* (struct gsl-rng)))
  (nu double)) ; degrees of freedom

;;; (gsl-ran-tdist-pdf x nu)
;;;   This function computes the probability density p(x) at x for a t-distribution with nu
;;;   degrees of freedom.
(define-alien-routine gsl-ran-tdist-pdf
    double
  (x double)
  (nu double))

;;; (gsl-cdf-tdist-p x nu)
;;; (gsl-cdf-tdist-q x nu)
;;; (gsl-cdf-tdist-pinv p nu)
;;; (gsl-cdf-tdist-qinv q nu)
;;;   These functions compute the cumulative distribution function P(x), Q(x) and their
;;;   inverses for the t-distribution with nu degrees of freedom.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-tdist-p for c-function "gsl_cdf_tdist_P".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-tdist-p))
(defun gsl-cdf-tdist-p (x nu)
  (with-alien ((gsl-cdf-tdist-p (function double double double)
                                :extern "gsl_cdf_tdist_P"))
    (values (alien-funcall gsl-cdf-tdist-p x nu))))

;; function gsl-cdf-tdist-q for c-function "gsl_cdf_tdist_Q".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-tdist-q))
  (defun gsl-cdf-tdist-q (x nu)
    (with-alien ((gsl-cdf-tdist-q (function double double double)
                                  :extern "gsl_cdf_tdist_Q"))
      (values (alien-funcall gsl-cdf-tdist-q x nu))))

;; function gsl-cdf-tdist-pinv for c-function "gsl_cdf_tdist_Pinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-tdist-pinv))
(defun gsl-cdf-tdist-pinv (p nu)
  (with-alien ((gsl-cdf-tdist-pinv (function double double double)
                                   :extern "gsl_cdf_tdist_Pinv"))
    (values (alien-funcall gsl-cdf-tdist-pinv p nu))))

;; function gsl-cdf-tdist-qinv for c-function "gsl_cdf_tdist_Qinv".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-tdist-qinv))
(defun gsl-cdf-tdist-qinv (q nu)
  (with-alien ((gsl-cdf-tdist-qinv (function double double double)
                                   :extern "gsl_cdf_tdist_Qinv"))
    (values (alien-funcall gsl-cdf-tdist-qinv q nu))))

;;; Spherical Vector Deistributions
;;;
;;; The spherical distributions generate random vectors located on a spherical surface.
;;; They can be used as random directions, for example in the steps of a random walk.

;;; (gsl-ran-dir-2d rng x y)
;;; (gsl-ran-dir-2d-trig-method rng x y)
;;;   This function returns a random direction vector v = (x, y) in two dimensions. The
;;;   vector is normalized such that |v|^2 = x^2 + y^2 = 1. The obvious way to do this is
;;;   to take a uniform random number between 0 and 2 * pi and let x and y be the sine
;;;   and cosine respectively. Two trig functions would have been expensive in the old
;;;   days, but with modern hardware implementations, this is sometimes the fastest way
;;;   to go. This is the case for the Pentium (but not the case for the Sum Sparcstation).
;;;   One can avoid the trig evaluations by choosing x and y in the interior of a unit
;;;   circle (choose them at random from the interior of the enclosing square, and then
;;;   reject those that are outside the unit circle), and dividing by sqrt(x^2 + y^2).
(define-alien-routine gsl-ran-dir-2d
    void
  (rng (* (struct gsl-rng)))
  (x (* double))
  (y (* double)))

(define-alien-routine gsl-ran-dir-2d-trig-method
    void
  (rng (* (struct gsl-rng)))
  (x (* double))
  (y (* double)))

;;; The Poission Distribution

;;; (gsl-ran-poisson rng mu)
;;;   This function returns a random integer from the Poisson distribution with mean mu.
(define-alien-routine gsl-ran-poisson
    unsigned-int
  (rng (* (struct gsl-rng)))
  (mu double))

;;; (gsl-ran-possion-pdf k mu)
;;;   This function computes the probability p(k) of obtaining k from a Poisson distribution
;;;   with mean mu.
(define-alien-routine gsl-ran-poisson-pdf
    double
  (k unsigned-int)
  (mu double))

;;; (gsl-cdf-poisson-p k mu)
;;; (gsl-cdf-poisson-q k mu)
;;;   These functions compute the cumulative distribution function P(k), Q(k) for the
;;;   Poisson distribution with parameter mu.
;;;
;;; define-alien-routine macro automatically try to find c-function name with lower case
;;; letter. If symbol is gsl-cdf-gaussian-p, macro try to find c-function gsl_cdf_gaussian_p.
;;; But cdf functions have partially upper case letter, so these functions are directly
;;; difined by defun.

;; function gsl-cdf-poisson-p for c-function "gsl_cdf_poisson_P".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-poisson-p))
(defun gsl-cdf-poisson-p (k mu)
  (with-alien ((gsl-cdf-poisson-p (function double unsigned-int double)
                                  :extern "gsl_cdf_poisson_P"))
    (values (alien-funcall gsl-cdf-poisson-p k mu))))

;; function gsl-cdf-poisson-q for c-function "gsl_cdf_poisson_Q".
(declaim (ftype (function (t t) (values (alien double) &optional)) gsl-cdf-poisson-q))
(defun gsl-cdf-poisson-q (k mu)
  (with-alien ((gsl-cdf-poisson-q (function double unsigned-int double)
                                  :extern "gsl_cdf_poisson_Q"))
    (values (alien-funcall gsl-cdf-poisson-q k mu))))

;;; The Bernoulli Distribution

;;; (gsl-ran-bernoulli rng p)
;;;   This function returns either 0 or 1, the result of a Bernoulli trial with probability p.
(define-alien-routine gsl-ran-bernoulli
    unsigned-int
  (rng (* (struct gsl-rng)))
  (p double))

;;; (gsl-ran-bernoulli-pdf k p)
;;;   This function computes the probability p(k) of obtaining k from a Bernoulli distribution
;;;   with probability parameter p.
(define-alien-routine gsl-ran-bernoulli-pdf
    double
  (k unsigned-int)
  (p double))

;;; The Binomial Distribution

;;; (gsl-ran-binomial rng p n)
;;;   This function returns a random integer from the binomial distribution, the number
;;;   of successes in n independent trials with probability p.
(define-alien-routine gsl-ran-binomial
    unsigned-int
  (rng (* (struct gsl-rng)))
  (p double)
  (n unsigned-int))

;;; (gsl-ran-binomial-pdf k p n)
;;;   This function computes the probability p(k) of obtaining k from a binomial distribution
;;;   with parameters p and n.
(define-alien-routine gsl-ran-binomial-pdf
    double
  (k unsigned-int)
  (p double)
  (n unsigned-int))

;;; (gsl-cdf-binomial-p k p n)
;;; (gsl-cdf-binomial-q k p n)
;;;   These functions compute the cumulative distribution functions P(k), Q(k) for the
;;;   binomial distribution with parameters p and n.

;; function gsl-cdf-binomial-p for c-function "gsl_cdf_binomial_P".
(declaim (ftype (function (t t t) (values (alien double) &optional)) gsl-cdf-binomial-p))
(defun gsl-cdf-binomial-p (k p n)
  (with-alien ((gsl-cdf-binomial-p (function double unsigned-int double unsigned-int)
                                   :extern "gsl_cdf_binomial_P"))
    (values (alien-funcall gsl-cdf-binomial-p k p n))))

;; function gsl-cdf-binomial-q for c-function "gsl_cdf_binomial_Q".
(declaim (ftype (function (t t t) (values (alien double) &optional)) gsl-cdf-binomial-q))
(defun gsl-cdf-binomial-q (k p n)
  (with-alien ((gsl-cdf-binomial-q (function double unsigned-int double unsigned-int)
                                   :extern "gsl_cdf_binomial_Q"))
    (values (alien-funcall gsl-cdf-binomial-q k p n))))

;;; Shuffling and Sampling

;;; (gsl-ran-shuffle rng base n size)
;;;   This function randomly shuffles the order of n objects, each of size size, stored in the
;;;   array base[0,...,n-1]. The output of the random number generator r is used to produce
;;;   the permutation. The algorithm generates all possible n! permutations with equal
;;;   probability, assuming a perfect source of random numbers.
(define-alien-routine gsl-ran-shuffle
    void
  (rng (* (struct gsl-rng)))
  (base (* t))
  (n size-t)
  (size size-t))

;;; rng

(defun gen-ran (acc fn &rest args)
  "This function returns n random variates using gsl random umber generation.
If it is needed to chage type and seed, set the environment variables
GSL_RNG_TYPE and GSL_RNG_SEED."
  (with-alien ((rng-type (* (struct gsl-rng-type)))
               (rng (* (struct gsl-rng))))
    (gsl-rng-env-setup)
    (setf rng-type gsl-rng-default)
    (setf rng (gsl-rng-alloc rng-type))
    (dotimes (i (length acc))
      (setf (aref acc i) (apply fn (cons rng args))))
    (gsl-rng-free rng)
    acc))

(defun gen-pair-ran (acc fn &rest args)
  "This function returns n random pair-variates using gsl random umber generation.
If it is needed to chage type and seed, set the environment variables
GSL_RNG_TYPE and GSL_RNG_SEED."
  (with-alien ((rng-type (* (struct gsl-rng-type)))
               (rng (* (struct gsl-rng)))
               (x double)
               (y double))
    (gsl-rng-env-setup)
    (setf rng-type gsl-rng-default)
    (setf rng (gsl-rng-alloc rng-type))
    (dotimes (i (car (array-dimensions acc)))
      (apply fn (cons rng (append args (list (addr x) (addr y)))))
      (setf (aref acc i 0) x)
      (setf (aref acc i 1) y))
    (gsl-rng-free rng)
    acc))

;;; The Gaussian Distribution

(defun ran-gaussian (n sigma)
  "This function returns n Gaussian random variates, with mean zero and standard
deviation sigma. Use the transformation z = mu + x on the numbers returned
by ran-gaussian to obtain a Gaussian distribution with mean mu."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-gaussian (coerce sigma 'double-float))))

(defun ran-gaussian-pdf (x sigma)
  "This function computes the probability density p(x) at x for a Gaussian distribution
with standard deviation sigma."
  (gsl-ran-gaussian-pdf (coerce x 'double-float) (coerce sigma 'double-float)))

(defun ran-gaussian-ziggurat (n sigma)
  "This function computes n Gaussian random variates using the alternative Marsaglia-
Tsang ziggurat methods. The Ziggurat algorithm is the fastest available algorithm
in most cases."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-gaussian-ziggurat (coerce sigma 'double-float))))

(defun ran-gaussian-ratio-method (n sigma)
  "This function computes n Gaussian random variates using the alternative Kinderman-
Monahoan-Leva ratio methods."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-gaussian-ratio-method (coerce sigma 'double-float))))

(defun ran-ugaussian (n)
  "This function returns n unit Gaussian random variates, with mean zero and standard
deviation sigma zero."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-ugaussian)))

(defun ran-ugaussian-pdf (x)
  "This function computes the probability density p(x) at x for the unit Gaussian
distribution."
  (gsl-ran-ugaussian-pdf (coerce x 'double-float)))

(defun ran-ugaussian-ratio-method (n)
  "This function computes n unit Gaussian random variates using the alternative Kinderman-
Monahoan-Leva ratio methods."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-ugaussian-ratio-method)))

(defun cdf-gaussian-p (x sigma)
  "This function compute the cumulative distribution functions P(x) for the Gaussian
distribution with standard deviation sigma."
  (gsl-cdf-gaussian-p (coerce x 'double-float) (coerce sigma 'double-float)))

(defun cdf-gaussian-q (x sigma)
  "This function compute the cumulative distribution functions Q(x) for the Gaussian
distribution with standard deviation sigma."
  (gsl-cdf-gaussian-q (coerce x 'double-float) (coerce sigma 'double-float)))

(defun cdf-gaussian-pinv (p sigma)
  "This function compute the cumulative distribution functions P(x) inverses for the
Gaussian distribution with standard deviation sigma."
  (gsl-cdf-gaussian-pinv (coerce p 'double-float) (coerce sigma 'double-float)))

(defun cdf-gaussian-qinv (q sigma)
  "This function compute the cumulative distribution functions Q(x) inverses for the
Gaussian distribution with standard deviation sigma."
  (gsl-cdf-gaussian-qinv (coerce q 'double-float) (coerce sigma 'double-float)))

(defun cdf-ugaussian-p (x)
  "This function compute the cumulative distribution functions P(x) for the unit
Gaussian distribution."
  (gsl-cdf-ugaussian-p (coerce x 'double-float)))

(defun cdf-ugaussian-q (x)
  "This function compute the cumulative distribution functions Q(x) for the unit
Gaussian distribution."
  (gsl-cdf-ugaussian-q (coerce x 'double-float)))

(defun cdf-ugaussian-pinv (p)
  "This function compute the cumulative distribution functions P(x) inverses for
the unit Gaussian distribution."
  (gsl-cdf-ugaussian-pinv (coerce p 'double-float)))

(defun cdf-ugaussian-qinv (q)
  "This function compute the cumulative distribution functions Q(x) inverses for
the unit Gaussian distribution."
  (gsl-cdf-ugaussian-qinv (coerce q 'double-float)))

;;; The Gaussian Tail Distribution

(defun ran-gaussian-tail (n a sigma)
  "This function provides random variates from the upper tail of a Gaussian distribution
with standard deviation sigma. The values returned are lagger than the lower limit a,
which must be positive. The method is based on Marsaglia's famous rectangle-wedge-tail
algorithm."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc
             #'gsl-ran-gaussian-tail
             (coerce a 'double-float)
             (coerce sigma 'double-float))))

(defun ran-gaussian-tail-pdf (x a sigma)
  "This function computes the probability density p(x) at x for Gaussian tail distribution
with standard deviation sigma and lower limit a."
  (gsl-ran-gaussian-tail-pdf (coerce x 'double-float)
                             (coerce a 'double-float)
                             (coerce sigma 'double-float)))

(defun ran-ugaussian-tail (n a)
  "This function compute result for the tail of a unit Gaussian distribution."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-ugaussian-tail (coerce a 'double-float))))

(defun ran-ugaussian-tail-pdf (x a)
  "This function compute result for the tail of a unit Gaussian distribution."
  (gsl-ran-ugaussian-tail-pdf (coerce x 'double-float) (coerce a 'double-float)))

;;; The Bivariate Gaussian Distribution

(defun ran-bivariate-gaussian (n sigma-x sigma-y rho)
  "This function generates a pair of correlated Gaussian variates, with mean zero,
correlation coefficient rho and standard deviations sigma-x and sigma-y in the x
and y directions."
  (let ((acc (make-array (list n 2)
                         :initial-element 0.0d0
                         :element-type 'double-float)))
    (gen-pair-ran acc
                  #'gsl-ran-bivariate-gaussian
                  (coerce sigma-x 'double-float)
                  (coerce sigma-y 'double-float)
                  (coerce rho 'double-float))))

(defun ran-bivariate-gaussian-pdf (x y sigma-x sigma-y rho)
  "This function computes the probability density p(x, y) at (x, y) for bivariate
Gaussian distribution with standard deviations sigma-x, sigma-y and correlation
coefficient rho."
  (gsl-ran-bivariate-gaussian-pdf (coerce x 'double-float)
                                  (coerce y 'double-float)
                                  (coerce sigma-x 'double-float)
                                  (coerce sigma-y 'double-float)
                                  (coerce rho 'double-float)))

;;; The Exponential Distribution

(defun ran-exponential (n mu)
  "This function returns a random variate from the exponential distribution  with mean mu."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-exponential (coerce mu 'double-float))))

(defun ran-exponential-pdf (x mu)
  "This function computes the probability density p(x) at x for an exponential distribution
with mean mu."
  (gsl-ran-exponential-pdf (coerce x 'double-float) (coerce mu 'double-float)))

(defun cdf-exponential-p (x mu)
  "This function compute the cumulative distribution functions P(x) for the exponential
distribution with mean mu."
  (gsl-cdf-exponential-p (coerce x 'double-float) (coerce mu 'double-float)))

(defun cdf-exponential-q (x mu)
  "This function compute the cumulative distribution functions Q(x) for the exponential
distribution with mean mu."
  (gsl-cdf-exponential-q (coerce x 'double-float) (coerce mu 'double-float)))

(defun cdf-exponential-pinv (p mu)
  "This function compute the cumulative distribution functions P(x) inverses for the
exponential distribution with mean mu."
  (gsl-cdf-exponential-pinv (coerce p 'double-float) (coerce mu 'double-float)))

(defun cdf-exponential-qinv (q mu)
  "This function compute the cumulative distribution functions Q(x) inverses for the
exponential distribution with mean mu."
  (gsl-cdf-exponential-qinv (coerce q 'double-float) (coerce mu 'double-float)))

;;; The Laplace Distribution

(defun ran-laplace (n a)
  "This function returns n random variates from the Laplace distribution with width a."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-laplace (coerce a 'double-float))))

(defun ran-laplace-pdf (x a)
  "This function computes the probability density p(x) at x for a Laplace distribution
with width a."
  (gsl-ran-laplace-pdf (coerce x 'double-float) (coerce a 'double-float)))

(defun cdf-laplace-p (x a)
  "This function compute the cumulative distribution function P(x) for the Laplace
distribution with width a."
  (gsl-cdf-laplace-p (coerce x 'double-float) (coerce a 'double-float)))

(defun cdf-laplace-q (x a)
  "This function compute the cumulative distribution function Q(x) for the Laplace
distribution with width a."
  (gsl-cdf-laplace-q (coerce x 'double-float) (coerce a 'double-float)))

(defun cdf-laplace-pinv (p a)
  "This function compute the cumulative distribution function P(x) inverses for
the Laplace distribution with width a."
  (gsl-cdf-laplace-pinv (coerce p 'double-float) (coerce a 'double-float)))

(defun cdf-laplace-qinv (q a)
  "This function compute the cumulative distribution function Q(x) inverses for
the Laplace distribution with width a."
  (gsl-cdf-laplace-qinv (coerce q 'double-float) (coerce a 'double-float)))

;;; The chi-squared distribution

(defun ran-chisq (n nu)
  "This function returns n random variate from the chi-squared distribution with nu
degrees of freedom."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-chisq (coerce nu 'double-float))))

(defun ran-chisq-pdf (x nu)
  "This function computes the probability density p(x) at x for a chi-squared distribution
with nu degrees of freedom."
  (gsl-ran-chisq-pdf (coerce x 'double-float) (coerce nu 'double-float)))

(defun cdf-chisq-p (x nu)
  "This function compute the cumulative distribution functions P(x)
for the chi-squared distribution with nu degrees of freedom."
  (gsl-cdf-chisq-p (coerce x 'double-float) (coerce nu 'double-float)))

(defun cdf-chisq-q (x nu)
  "This function compute the cumulative distribution functions Q(x)
for the chi-squared distribution with nu degrees of freedom."
  (gsl-cdf-chisq-q (coerce x 'double-float) (coerce nu 'double-float)))

(defun cdf-chisq-pinv (p nu)
  "These functions compute the cumulative distribution functions P(x) inverses
for the chi-squared distribution with nu degrees of freedom."
  (gsl-cdf-chisq-pinv (coerce p 'double-float) (coerce nu 'double-float)))

(defun cdf-chisq-qinv (q nu)
  "These functions compute the cumulative distribution functions Q(x) inverses
for the chi-squared distribution with nu degrees of freedom."
  (gsl-cdf-chisq-qinv (coerce q 'double-float) (coerce nu 'double-float)))

;;; The F-distribution

(defun ran-fdist (n nu1 nu2)
  "This function returns n random variate from the F-distribution with degrees of
freedom nu1 and nu2."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc
             #'gsl-ran-fdist
             (coerce nu1 'double-float)
             (coerce nu2 'double-float))))

(defun ran-fdist-pdf (x nu1 nu2)
  "This function computes the probability density p(x) at x for an F-distribution with
nu1 and nu2 degrees of freedom."
  (gsl-ran-fdist-pdf (coerce x 'double-float)
                     (coerce nu1 'double-float)
                     (coerce nu2 'double-float)))

(defun cdf-fdist-p (x nu1 nu2)
  "These functions compute the cumulative distribution functions P(x)
for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl-cdf-fdist-p (coerce x 'double-float)
                   (coerce nu1 'double-float)
                   (coerce nu2 'double-float)))

(defun cdf-fdist-q (x nu1 nu2)
  "These functions compute the cumulative distribution functions Q(x)
for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl-cdf-fdist-q (coerce x 'double-float)
                   (coerce nu1 'double-float)
                   (coerce nu2 'double-float)))

(defun cdf-fdist-pinv (p nu1 nu2)
  "These functions compute the cumulative distribution functions P(x) inverses
for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl-cdf-fdist-pinv (coerce p 'double-float)
                      (coerce nu1 'double-float)
                      (coerce nu2 'double-float)))

(defun cdf-fdist-qinv (q nu1 nu2)
  "These functions compute the cumulative distribution functions Q(x) inverses
for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl-cdf-fdist-qinv (coerce q 'double-float)
                      (coerce nu1 'double-float)
                      (coerce nu2 'double-float)))

;;; The t-distribution

(defun ran-tdist (n nu)
  "This function returns n random variates from the t-distribution."
  (let ((acc (make-array n :element-type 'double-float)))
    (gen-ran acc #'gsl-ran-tdist (coerce nu 'double-float))))

(defun ran-tdist-pdf (x nu)
  "This function computes the probability density p(x) at x for a t-distribution with nu
degrees of freedom."
  (gsl-ran-tdist-pdf (coerce x 'double-float) (coerce nu 'double-float)))

(defun cdf-tdist-p (x nu)
  "This function compute the cumulative distribution function P(x) for the
t-distribution with nu degrees of freedom."
  (gsl-cdf-tdist-p (coerce x 'double-float) (coerce nu 'double-float)))

(defun cdf-tdist-q (x nu)
  "This function compute the cumulative distribution function Q(x) for the
t-distribution with nu degrees of freedom."
  (gsl-cdf-tdist-q (coerce x 'double-float) (coerce nu 'double-float)))

(defun cdf-tdist-pinv (p nu)
  "This function compute the cumulative distribution function P(x) inverses for
the t-distribution with nu degrees of freedom."
  (gsl-cdf-tdist-pinv (coerce p 'double-float) (coerce nu 'double-float)))

(defun cdf-tdist-qinv (p nu)
  "This function compute the cumulative distribution function Q(x) inverses for
the t-distribution with nu degrees of freedom."
  (gsl-cdf-tdist-qinv (coerce p 'double-float) (coerce nu 'double-float)))

;;; Spherical Vector Deistributions

(defun ran-dir-2d (n)
  "This function returns a random direction vector v = (x, y) in two dimensions. The
vector is normalized such that |v|^2 = x^2 + y^2 = 1."
  (let ((acc (make-array (list n 2) :element-type 'double-float)))
    (gen-pair-ran acc #'gsl-ran-dir-2d)))

(defun ran-dir-2d-trig-method (n)
  "This function returns a random direction vector v = (x, y) in two dimensions. The
vector is normalized such that |v|^2 = x^2 + y^2 = 1."
  (let ((acc (make-array (list n 2) :element-type 'double-float)))
    (gen-pair-ran acc #'gsl-ran-dir-2d-trig-method)))

;;; The Poission Distribution

(defun ran-poisson (n mu)
  "This function returns a random integer from the Poisson distribution with mean mu."
  (let ((acc (make-array n :element-type `(unsigned-byte ,(alien-size unsigned-int)))))
    (gen-ran acc #'gsl-ran-poisson (coerce mu 'double-float))))

(defun ran-poisson-pdf (k mu)
  "This function computes the probability p(k) of obtaining k from a Poisson distribution
with mean mu."
  (gsl-ran-poisson-pdf (coerce k '(unsigned-byte 16)) (coerce mu 'double-float)))

(defun cdf-poisson-p (k mu)
  "This functions compute the cumulative distribution function P(k) for the
Poisson distribution with parameter mu."
  (gsl-cdf-poisson-p (coerce k '(unsigned-byte 16)) (coerce mu 'double-float)))

(defun cdf-poisson-q (k mu)
  "This functions compute the cumulative distribution function Q(k) for the
Poisson distribution with parameter mu."
  (gsl-cdf-poisson-q (coerce k '(unsigned-byte 16)) (coerce mu 'double-float)))

;;; The Bernoulli Distribution

(defun ran-bernoulli (n p)
  "This function returns either 0 or 1, the result of a Bernoulli trial with probability p."
  (let ((acc (make-array n :element-type `(unsigned-byte ,(alien-size unsigned-int)))))
    (gen-ran acc #'gsl-ran-bernoulli (coerce p 'double-float))))

(defun ran-bernoulli-pdf (k p)
  "This function computes the probability p(k) of obtaining k from a Bernoulli distribution
with probability parameter p."
  (gsl-ran-bernoulli-pdf (coerce k '(unsigned-byte 16)) (coerce p 'double-float)))

;;; The Binomial Distribution

(defun ran-binomial (n p size)
  "This function returns m random integers from the binomial distribution, the number
of successes in n independent trials with probability p."
  (let ((acc (make-array n :element-type `(unsigned-byte ,(alien-size unsigned-int)))))
    (gen-ran acc
             #'gsl-ran-binomial
             (coerce p 'double-float)
             (coerce size '(unsigned-byte 16)))))

(defun ran-binomial-pdf (k p size)
  "This function computes the probability p(k) of obtaining k from a binomial distribution
with parameters p and n."
  (gsl-ran-binomial-pdf (coerce k '(unsigned-byte 16))
                        (coerce p 'double-float)
                        (coerce size '(unsigned-byte 16))))

(defun cdf-binomial-p (k p size)
  "This functions compute the cumulative distribution functions P(k) for the
binomial distribution with parameters p and n."
  (gsl-cdf-binomial-p (coerce k '(unsigned-byte 16))
                      (coerce p 'double-float)
                      (coerce size '(unsigned-byte 16))))

(defun cdf-binomial-q (k p size)
  "This functions compute the cumulative distribution functions Q(k) for the
binomial distribution with parameters p and n."
  (gsl-cdf-binomial-q (coerce k '(unsigned-byte 16))
                      (coerce p 'double-float)
                      (coerce size '(unsigned-byte 16))))
