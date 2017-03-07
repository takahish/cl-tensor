;;;; gsl/qrng-type.lisp
;;;;
;;;; This file describes functions for generating quasi-random
;;;; sequences in arbitrary dimensions. A quasi-random sequence
;;;; progressively covers a d-dimensional space with a set of points
;;;; that are uniformly distributed.  Quasi-random sequences are also
;;;; known as low-discrepancy sequences. The quasi-random sequence
;;;; generators use an interface that is similar to the interface for
;;;; random number generators, except that seeding is not required
;;;; each generator produces a single sequence.

;;;; Copyright (C) 2016, 2017 Takahiro Ishikawa
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


;;; qrng

(defclass qrng ()
  ((pointer :accessor pointer :initarg :pointer)))

(defvar *qrng-default* gsl_qrng_sobol
  "Available gsl_qrng_type:
  gsl_qrng_niederreiter_2
  gsl_qrng_sobol (default)
  gsl_qrng_halton
  gsl_qrng_reversehalton")

(defun qrng-alloc (&optional (dimension 2) (type *qrng-default*))
  "This function returns a pointer to a newly-created instance of a
quasi-random sequence generator of type and dimension. If there is
insufficient memory to create the generator then the function returns
a null pointer and the error handler is invoked with an error code of
GSL_ENOMEM."
  (make-instance 'qrng :pointer (gsl_qrng_alloc type dimension)))

(defvar *qrng* (qrng-alloc))

(defun qrng-free (&optional (qrng *qrng*) (result nil))
  "This function frees all the memory associated with the generator
qrng."
  (gsl_qrng_free (pointer qrng))
  result)

(defun qrng-init (&optional (qrng *qrng*))
  "This function reinitializes the generator qrng to its starting
point. Note that quasi-random sequences do not use a seed and always
produce the same set of values."
  (gsl_qrng_init (pointer qrng))
  qrng)
