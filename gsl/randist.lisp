;;;; cl-scl/gsl/randist.lisp
;;;;
;;;; This file describes for generating random variates and computing
;;;; their probability distributions. Samples from the distributions
;;;; described in this file can be obtained using any of the random
;;;; number generators in the library as an underlying source of
;;;; randomness.

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


(defun gen-ran (n fn &rest args)
  "This function returns n random variates using gsl random umber
generation. If it is needed to chage type and seed, set the
environment variables GSL_RNG_TYPE and GSL_RNG_SEED."
  (if (or (null n) (< n 1))
      (apply fn args)
      (let ((acc nil))
        (dotimes (i n (nreverse acc))
          (push (apply fn args) acc)))))

(defun gen-pair-ran (n fn &rest args)
  "This function returns n random pair-variates using gsl random umber
generation. If it is needed to chage type and seed, set the
environment variables GSL_RNG_TYPE and GSL_RNG_SEED."
  (cffi:with-foreign-objects ((x :double) (y :double))
    (setf args (append args (list x y)))
    (if (or (null n) (< n 1))
        (progn (apply fn args)
               (values (cffi:mem-ref x :double)
                       (cffi:mem-ref y :double)))
        (let ((acc nil))
          (dotimes (i n (nreverse acc))
            (apply fn args)
            (push (list (cffi:mem-ref x :double)
                        (cffi:mem-ref y :double))
                  acc))))))

;;; The Gaussian Distribution

(defun ran-gaussian (sigma &optional (n nil) (rng *rng*))
  "This function returns n Gaussian random variates, with mean zero
and standard deviation sigma. Use the transformation z = mu + x on the
numbers returned by gsl-ran-gaussian to obtain a Gaussian distribution
with mean mu. This function uses the Box-Muller algorithm which
requires two calls to the random number generator rng."
  (gen-ran n #'gsl_ran_gaussian (pointer rng)
           (coerce sigma 'double-float)))

(defun ran-gaussian-pdf (x sigma)
  "This function computes the probability density p(x) at x for a
Gaussian distribution with standard deviation sigma."
  (gsl_ran_gaussian_pdf (coerce x 'double-float)
                        (coerce sigma 'double-float)))

(defun ran-gaussian-ziggurat (sigma &optional (n nil) (rng *rng*))
  "This function computes n Gaussian random variates using the
alternative Marsaglia- Tsang ziggurat methods. The Ziggurat algorithm
is the fastest available algorithm in most cases."
  (gen-ran n #'gsl_ran_gaussian_ziggurat (pointer rng)
           (coerce sigma 'double-float)))

(defun ran-gaussian-ratio-method (sigma &optional (n nil) (rng *rng*))
  "This function computes n Gaussian random variates using the
alternative Kinderman- Monahoan-Leva ratio methods."
  (gen-ran n #'gsl_ran_gaussian_ratio_method (pointer rng)
           (coerce sigma 'double-float)))

(defun ran-ugaussian (&optional (n nil) (rng *rng*))
  "This function returns n unit Gaussian random variates, with mean
zero and standard deviation sigma zero."
  (gen-ran n #'gsl_ran_ugaussian (pointer rng)))

(defun ran-ugaussian-pdf (x)
  "This function computes the probability density p(x) at x for the
unit Gaussian distribution."
  (gsl_ran_ugaussian_pdf (coerce x 'double-float)))

(defun ran-ugaussian-ratio-method (&optional (n nil) (rng *rng*))
  "This function computes n unit Gaussian random variates using the
alternative Kinderman- Monahoan-Leva ratio methods."
  (gen-ran n #'gsl_ran_ugaussian_ratio_method (pointer rng)))

(defun cdf-gaussian-p (x sigma)
  "This function compute the cumulative distribution functions P(x)
for the Gaussian distribution with standard deviation sigma."
  (gsl_cdf_gaussian_P (coerce x 'double-float)
                      (coerce sigma 'double-float)))

(defun cdf-gaussian-q (x sigma)
  "This function compute the cumulative distribution functions Q(x)
for the Gaussian distribution with standard deviation sigma."
  (gsl_cdf_gaussian_q (coerce x 'double-float)
                      (coerce sigma 'double-float)))

(defun cdf-gaussian-pinv (p sigma)
  "This function compute the cumulative distribution functions P(x)
inverses for the Gaussian distribution with standard deviation sigma."
  (gsl_cdf_gaussian_Pinv (coerce p 'double-float)
                         (coerce sigma 'double-float)))

(defun cdf-gaussian-qinv (q sigma)
  "This function compute the cumulative distribution functions Q(x)
inverses for the Gaussian distribution with standard deviation sigma."
  (gsl_cdf_gaussian_Qinv (coerce q 'double-float)
                         (coerce sigma 'double-float)))

(defun cdf-ugaussian-p (x)
  "This function compute the cumulative distribution functions P(x)
for the unit Gaussian distribution."
  (gsl_cdf_ugaussian_P (coerce x 'double-float)))

(defun cdf-ugaussian-q (x)
  "This function compute the cumulative distribution functions Q(x)
for the unit Gaussian distribution."
  (gsl_cdf_ugaussian_Q (coerce x 'double-float)))

(defun cdf-ugaussian-pinv (p)
  "This function compute the cumulative distribution functions P(x)
inverses for the unit Gaussian distribution."
  (gsl_cdf_ugaussian_Pinv (coerce p 'double-float)))

(defun cdf-ugaussian-qinv (q)
  "This function compute the cumulative distribution functions Q(x)
inverses for the unit Gaussian distribution."
  (gsl_cdf_ugaussian_Qinv (coerce q 'double-float)))

;;; The Gaussian Tail Distribution

(defun ran-gaussian-tail (a sigma &optional (n nil) (rng *rng*))
  "This function provides random variates from the upper tail of a
Gaussian distribution with standard deviation sigma. The values
returned are lagger than the lower limit a, which must be
positive. The method is based on Marsaglia's famous
rectangle-wedge-tail algorithm."
  (gen-ran n #'gsl_ran_gaussian_tail (pointer rng)
           (coerce a 'double-float) (coerce sigma 'double-float)))

(defun ran-gaussian-tail-pdf (x a sigma)
  "This function computes the probability density p(x) at x for
Gaussian tail distribution with standard deviation sigma and lower
limit a."
  (gsl_ran_gaussian_tail_pdf (coerce x 'double-float)
                             (coerce a 'double-float)
                             (coerce sigma 'double-float)))

(defun ran-ugaussian-tail (a &optional (n nil) (rng *rng*))
  "This function compute result for the tail of a unit Gaussian
distribution."
  (gen-ran n #'gsl_ran_ugaussian_tail (pointer rng)
           (coerce a 'double-float)))

(defun ran-ugaussian-tail-pdf (x a)
  "This function compute result for the tail of a unit Gaussian
distribution."
  (gsl_ran_ugaussian_tail_pdf (coerce x 'double-float)
                              (coerce a 'double-float)))

;;; The Bivariate Gaussian Distribution

(defun ran-bivariate-gaussian (sigma-x sigma-y rho &optional (n nil) (rng *rng*))
  "This function generates a pair of correlated Gaussian variates,
with mean zero, correlation coefficient rho and standard deviations
sigma-x and sigma-y in the x and y directions."
  (gen-pair-ran n #'gsl_ran_bivariate_gaussian (pointer rng)
                (coerce sigma-x 'double-float) (coerce sigma-y 'double-float)
                (coerce rho 'double-float)))

(defun ran-bivariate-gaussian-pdf (x y sigma-x sigma-y rho)
  "This function computes the probability density p(x, y) at (x, y)
for bivariate Gaussian distribution with standard deviations sigma-x,
sigma-y and correlation coefficient rho."
  (gsl_ran_bivariate_gaussian_pdf (coerce x 'double-float)
                                  (coerce y 'double-float)
                                  (coerce sigma-x 'double-float)
                                  (coerce sigma-y 'double-float)
                                  (coerce rho 'double-float)))

;;; The Exponential Distribution

(defun ran-exponential (mu &optional (n nil) (rng *rng*))
  "This function returns a random variate from the exponential
distribution with mean mu."
  (gen-ran n #'gsl_ran_exponential (pointer rng)
           (coerce mu 'double-float)))

(defun ran-exponential-pdf (x mu)
  "This function computes the probability density p(x) at x for an
exponential distribution with mean mu."
  (gsl_ran_exponential_pdf (coerce x 'double-float)
                           (coerce mu 'double-float)))

(defun cdf-exponential-p (x mu)
  "This function compute the cumulative distribution functions P(x)
for the exponential distribution with mean mu."
  (gsl_cdf_exponential_P (coerce x 'double-float)
                         (coerce mu 'double-float)))

(defun cdf-exponential-q (x mu)
  "This function compute the cumulative distribution functions Q(x)
for the exponential distribution with mean mu."
  (gsl_cdf_exponential_Q (coerce x 'double-float)
                         (coerce mu 'double-float)))

(defun cdf-exponential-pinv (p mu)
  "This function compute the cumulative distribution functions P(x)
inverses for the exponential distribution with mean mu."
  (gsl_cdf_exponential_Pinv (coerce p 'double-float)
                            (coerce mu 'double-float)))

(defun cdf-exponential-qinv (q mu)
  "This function compute the cumulative distribution functions Q(x)
inverses for the exponential distribution with mean mu."
  (gsl_cdf_exponential_Qinv (coerce q 'double-float)
                            (coerce mu 'double-float)))

;;; The Laplace Distribution

(defun ran-laplace (a &optional (n nil) (rng *rng*))
  "This function returns n random variates from the Laplace
distribution with width a."
  (gen-ran n #'gsl_ran_laplace (pointer rng)
           (coerce a 'double-float)))

(defun ran-laplace-pdf (x a)
  "This function computes the probability density p(x) at x for a
Laplace distribution with width a."
  (gsl_ran_laplace_pdf (coerce x 'double-float)
                       (coerce a 'double-float)))

(defun cdf-laplace-p (x a)
  "This function compute the cumulative distribution function P(x) for
the Laplace distribution with width a."
  (gsl_cdf_laplace_P (coerce x 'double-float)
                     (coerce a 'double-float)))

(defun cdf-laplace-q (x a)
  "This function compute the cumulative distribution function Q(x) for
the Laplace distribution with width a."
  (gsl_cdf_laplace_Q (coerce x 'double-float)
                     (coerce a 'double-float)))

(defun cdf-laplace-pinv (p a)
  "This function compute the cumulative distribution function P(x)
inverses for the Laplace distribution with width a."
  (gsl_cdf_laplace_Pinv (coerce p 'double-float)
                        (coerce a 'double-float)))

(defun cdf-laplace-qinv (q a)
  "This function compute the cumulative distribution function Q(x)
inverses for the Laplace distribution with width a."
  (gsl_cdf_laplace_Qinv (coerce q 'double-float)
                        (coerce a 'double-float)))

;;; The chi-squared distribution

(defun ran-chisq (nu &optional (n nil) (rng *rng*))
  "This function returns n random variate from the chi-squared
distribution with nu degrees of freedom."
  (gen-ran n #'gsl_ran_chisq (pointer rng)
           (coerce nu 'double-float)))

(defun ran-chisq-pdf (x nu)
  "This function computes the probability density p(x) at x for a
chi-squared distribution with nu degrees of freedom."
  (gsl_ran_chisq_pdf (coerce x 'double-float)
                     (coerce nu 'double-float)))

(defun cdf-chisq-p (x nu)
  "This function compute the cumulative distribution functions P(x)
for the chi-squared distribution with nu degrees of freedom."
  (gsl_cdf_chisq_P (coerce x 'double-float)
                   (coerce nu 'double-float)))

(defun cdf-chisq-q (x nu)
  "This function compute the cumulative distribution functions Q(x)
for the chi-squared distribution with nu degrees of freedom."
  (gsl_cdf_chisq_Q (coerce x 'double-float)
                   (coerce nu 'double-float)))

(defun cdf-chisq-pinv (p nu)
  "These functions compute the cumulative distribution functions P(x)
inverses for the chi-squared distribution with nu degrees of freedom."
  (gsl_cdf_chisq_Pinv (coerce p 'double-float)
                      (coerce nu 'double-float)))

(defun cdf-chisq-qinv (q nu)
  "These functions compute the cumulative distribution functions Q(x)
inverses for the chi-squared distribution with nu degrees of freedom."
  (gsl_cdf_chisq_Qinv (coerce q 'double-float)
                      (coerce nu 'double-float)))

;;; The F-distribution

(defun ran-fdist (nu1 nu2 &optional (n nil) (rng *rng*))
  "This function returns n random variate from the F-distribution with
degrees of freedom nu1 and nu2."
  (gen-ran n #'gsl_ran_fdist (pointer rng)
           (coerce nu1 'double-float)
           (coerce nu2 'double-float)))

(defun ran-fdist-pdf (x nu1 nu2)
  "This function computes the probability density p(x) at x for an
F-distribution with nu1 and nu2 degrees of freedom."
  (gsl_ran_fdist_pdf (coerce x 'double-float)
                     (coerce nu1 'double-float)
                     (coerce nu2 'double-float)))

(defun cdf-fdist-p (x nu1 nu2)
  "These functions compute the cumulative distribution functions P(x)
for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl_cdf_fdist_P (coerce x 'double-float)
                   (coerce nu1 'double-float)
                   (coerce nu2 'double-float)))

(defun cdf-fdist-q (x nu1 nu2)
  "These functions compute the cumulative distribution functions Q(x)
for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl_cdf_fdist_Q (coerce x 'double-float)
                   (coerce nu1 'double-float)
                   (coerce nu2 'double-float)))

(defun cdf-fdist-pinv (p nu1 nu2)
  "These functions compute the cumulative distribution functions P(x)
inverses for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl_cdf_fdist_Pinv (coerce p 'double-float)
                      (coerce nu1 'double-float)
                      (coerce nu2 'double-float)))

(defun cdf-fdist-qinv (q nu1 nu2)
  "These functions compute the cumulative distribution functions Q(x)
inverses for the F-distribution with nu1 and nu2 degrees of freedom."
  (gsl_cdf_fdist_Qinv (coerce q 'double-float)
                      (coerce nu1 'double-float)
                      (coerce nu2 'double-float)))

;;; The t-distribution

(defun ran-tdist (nu &optional (n nil) (rng *rng*))
  "This function returns n random variates from the t-distribution."
  (gen-ran n #'gsl_ran_tdist (pointer rng)
           (coerce nu 'double-float)))

(defun ran-tdist-pdf (x nu)
  "This function computes the probability density p(x) at x for a
t-distribution with nu degrees of freedom."
  (gsl_ran_tdist_pdf (coerce x 'double-float)
                     (coerce nu 'double-float)))

(defun cdf-tdist-p (x nu)
  "This function compute the cumulative distribution function P(x) for
the t-distribution with nu degrees of freedom."
  (gsl_cdf_tdist_P (coerce x 'double-float)
                   (coerce nu 'double-float)))

(defun cdf-tdist-q (x nu)
  "This function compute the cumulative distribution function Q(x) for
the t-distribution with nu degrees of freedom."
  (gsl_cdf_tdist_Q (coerce x 'double-float)
                   (coerce nu 'double-float)))

(defun cdf-tdist-pinv (p nu)
  "This function compute the cumulative distribution function P(x)
inverses for the t-distribution with nu degrees of freedom."
  (gsl_cdf_tdist_Pinv (coerce p 'double-float)
                      (coerce nu 'double-float)))

(defun cdf-tdist-qinv (q nu)
  "This function compute the cumulative distribution function Q(x)
inverses for the t-distribution with nu degrees of freedom."
  (gsl_cdf_tdist_Qinv (coerce q 'double-float)
                      (coerce nu 'double-float)))

;;; Spherical Vector Deistributions
;;;
;;; The spherical distributions generate random vectors located on a
;;; spherical surface.  They can be used as random directions, for
;;; example in the steps of a random walk.

(defun ran-dir-2d (&optional (n nil) (rng *rng*))
  "This function returns a random direction vector v = (x, y) in two
dimensions. The vector is normalized such that |v|^2 = x^2 + y^2 =
1. The obvious way to do this is to take a uniform random number
between 0 and 2 * pi and let x and y be the sine and cosine
respectively. Two trig functions would have been expensive in the old
days, but with modern hardware implementations, this is sometimes the
fastest way to go. This is the case for the Pentium (but not the case
for the Sum Sparcstation).  One can avoid the trig evaluations by
choosing x and y in the interior of a unit circle (choose them at
random from the interior of the enclosing square, and then reject
those that are outside the unit circle), and dividing by sqrt(x^2 +
y^2)."
  (gen-pair-ran n #'gsl_ran_dir_2d (pointer rng)))

(defun ran-dir-2d-trig-method (&optional (n nil) (rng *rng*))
  "This function returns a random direction vector v = (x, y) in two
dimensions. The vector is normalized such that |v|^2 = x^2 + y^2 =
1. The obvious way to do this is to take a uniform random number
between 0 and 2 * pi and let x and y be the sine and cosine
respectively. Two trig functions would have been expensive in the old
days, but with modern hardware implementations, this is sometimes the
fastest way to go. This is the case for the Pentium (but not the case
for the Sum Sparcstation).  One can avoid the trig evaluations by
choosing x and y in the interior of a unit circle (choose them at
random from the interior of the enclosing square, and then reject
those that are outside the unit circle), and dividing by sqrt(x^2 +
y^2)."
  (gen-pair-ran n #'gsl_ran_dir_2d_trig_method (pointer rng)))

;;; The Poission Distribution

(defun ran-poisson (mu &optional (n nil) (rng *rng*))
  "This function returns a random integer from the Poisson
distribution with mean mu."
  (gen-ran n #'gsl_ran_poisson (pointer rng)
           (coerce mu 'double-float)))

(defun ran-poisson-pdf (k mu)
  "This function computes the probability p(k) of obtaining k from a
Poisson distribution with mean mu."
  (gsl_ran_poisson_pdf
   (coerce k `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))
   (coerce mu 'double-float)))

(defun cdf-poisson-p (k mu)
  "This functions compute the cumulative distribution function P(k)
for the Poisson distribution with parameter mu."
  (gsl_cdf_poisson_P
   (coerce k `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))
   (coerce mu 'double-float)))

(defun cdf-poisson-q (k mu)
  "This functions compute the cumulative distribution function Q(k)
for the Poisson distribution with parameter mu."
  (gsl_cdf_poisson_Q
   (coerce k `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))
   (coerce mu 'double-float)))

;;; The Bernoulli Distribution

(defun ran-bernoulli (p &optional (n nil) (rng *rng*))
  "This function returns either 0 or 1, the result of a Bernoulli
trial with probability p."
  (gen-ran n #'gsl_ran_bernoulli (pointer rng)
           (coerce p 'double-float)))

(defun ran-bernoulli-pdf (k p)
  "This function computes the probability p(k) of obtaining k from a
Bernoulli distribution with probability parameter p."
  (gsl_ran_bernoulli_pdf k p))

;;; The Binomial Distribution

(defun ran-binomial (p size &optional (n nil) (rng *rng*))
  "This function returns m random integers from the binomial
distribution, the number of successes in n independent trials with
probability p."
  (gen-ran n #'gsl_ran_binomial (pointer rng)
           (coerce p 'double-float)
           (coerce size `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))

(defun ran-binomial-pdf (k p size)
  "This function computes the probability p(k) of obtaining k from a
binomial distribution with parameters p and n."
  (gsl_ran_binomial_pdf
   (coerce k `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))
   (coerce p  'double-float)
   (coerce size `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))

(defun cdf-binomial-p (k p size)
  "This functions compute the cumulative distribution functions P(k)
for the binomial distribution with parameters p and n."
  (gsl_cdf_binomial_P
   (coerce k `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))
   (coerce p 'double-float)
   (coerce size `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))

(defun cdf-binomial-q (k p size)
  "This functions compute the cumulative distribution functions Q(k)
for the binomial distribution with parameters p and n."
  (gsl_cdf_binomial_Q
   (coerce k `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))
   (coerce p 'double-float)
   (coerce size `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))
