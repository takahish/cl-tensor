;;;; gsl/qrng.lisp
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


;;; function

(defun qrng-get-coordinates (&optional (dimension 2) (qrng *qrng*))
  "This function returns n quasi random variates. The space available
must match the dimension of the generator. gsl_qrng_get stores the
next point from the sequence generator qrng in the array x. The space
available for x must match the dimension of the generator. The point x
will lie in the range 0 < x[i] < 1 for each x[i]. An inline version of
this function is used when HAVE_INLINE is defined."
  (let ((coodinates nil))
    (cffi:with-foreign-object (x :double dimension)
      (gsl_qrng_get (pointer qrng) x)
      (dotimes (i dimension (nreverse coodinates))
        (push (cffi:mem-aref x :double i) coodinates)))))

(defun qrng-get (&optional n (dimension 2) (qrng *qrng*))
  (if (or (null n) (< n 1))
      (qrng-get-coordinates dimension qrng)
      (let ((acc nil))
        (dotimes (i n (nreverse acc))
          (push (qrng-get-coordinates dimension qrng) acc)))))

(defun qrng-name (&optional (qrng *qrng*))
  "This function returns a pointer to the name of the generator."
  (gsl_qrng_name (pointer qrng)))

(defun qrng-size (&optional (qrng *qrng*))
  "This functions return a pointer to the size."
  (gsl_qrng_size (pointer qrng)))

(defun qrng-state (&optional (qrng *qrng*))
  "This functions return a pointer to the state of generator qrng."
  (gsl_qrng_state (pointer qrng)))

(defun qrng-memcpy (dest &optional (src *qrng*))
  "This function copies the quasi-random sequence generator src into
the pre-existing generator dest, making dest into an exact copy of
src. The two generators must be of the same type."
  (gsl_qrng_memcpy (pointer dest) (pointer src))
  dest)

(defun qrng-clone (&optional (qrng *qrng*))
  "This function returns a pointer to a newly created generator which
is an exact copy of the generator qrng."
  (make-instance 'qrng :pointer (gsl_qrng_clone (pointer qrng))))
