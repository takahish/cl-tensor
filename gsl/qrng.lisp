;;;; cl-scl/gsl/qrng.lisp
;;;;
;;;; This file describes functions for generating quasi-random
;;;; sequences in arbitrary dimensions. A quasi-random sequence
;;;; progressively covers a d-dimensional space with a set of points
;;;; that are uniformly distributed.  Quasi-random sequences are also
;;;; known as low-discrepancy sequences. The quasi-random sequence
;;;; generators use an interface that is similar to the interface for
;;;; random number generators, except that seeding is not required
;;;; each generator produces a single sequence.

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

(defclass qrng ()
  ((entity :accessor entity :initarg :entity)))

(defvar *qrng-default* gsl_qrng_sobol
  "Available gsl_qrng_type:
  gsl_qrng_niederreiter_2
  gsl_qrng_sobol (default)
  gsl_qrng_halton
  gsl_qrng_reversehalton")

(defun qrng-alloc (type dimension)
  "This function returns a pointer to a newly-created instance of a
quasi-random sequence generator of type and dimension. If there is
insufficient memory to create the generator then the function returns
a null pointer and the error handler is invoked with an error code of
GSL_ENOMEM."
  (make-instance 'qrng :entity (gsl_qrng_alloc type dimension)))

(defun qrng-free (qrng &optional (result nil))
  "This function frees all the memory associated with the generator
qrng."
  (gsl_qrng_free (entity qrng))
  result)

(defun qrng-init (qrng)
  "This function reinitializes the generator qrng to its starting
point. Note that quasi-random sequences do not use a seed and always
produce the same set of values."
  (gsl_qrng_init (entity qrng))
  qrng)

(defun qrng-get (qrng dimension)
  "This function returns n quasi random variates. The space available
must match the dimension of the generator. gsl_qrng_get stores the
next point from the sequence generator qrng in the array x. The space
available for x must match the dimension of the generator. The point x
will lie in the range 0 < x[i] < 1 for each x[i]. An inline version of
this function is used when HAVE_INLINE is defined."
  (let ((acc nil))
    (cffi:with-foreign-object (x :double dimension)
      (gsl_qrng_get (entity qrng) x)
      (dotimes (i dimension (nreverse acc))
        (push (cffi:mem-aref x :double i) acc)))))

(defun qrng-name (qrng)
  "This function returns a pointer to the name of the generator."
  (gsl_qrng_name (entity qrng)))

(defun qrng-size (qrng)
  "This functions return a pointer to the size."
  (gsl_qrng_size (entity qrng)))

(defun qrng-state (qrng)
  "This functions return a pointer to the state of generator qrng."
  (gsl_qrng_state (entity qrng)))

(defun qrng-memcpy (dest src)
  "This function copies the quasi-random sequence generator src into
the pre-existing generator dest, making dest into an exact copy of
src. The two generators must be of the same type."
  (gsl_qrng_memcpy (entity dest) (entity src))
  dest)

(defun qrng-clone (qrng)
  "This function returns a pointer to a newly created generator which
is an exact copy of the generator qrng."
  (make-instance 'qrng :entity (gsl_qrng_clone (entity qrng))))
