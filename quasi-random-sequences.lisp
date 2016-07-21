;;;; sbcl-gsl/quasi-random-sequences.lisp
;;;;
;;;; This file describes functions for generating quasi-random sequences in
;;;; arbitrary dimensions. A quasi-random sequence progressively covers a
;;;; d-dimensional space with a set of points that are uniformly distributed.
;;;; Quasi-random sequences are also known as low-discrepancy sequences. The
;;;; quasi-random sequence generators use an interface that is similar to the
;;;; interface for random number generators, except that seeding is not required
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

(cl:defpackage "GSL-QRNG"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL")
  (:export "GSL-QRNG-TYPE"
           "GSL-QRNG"
           "GSL-QRNG-NIEDERREITER_2"
           "GSL-QRNG-SOBOL"
           "GSL-QRNG-HALTON"
           "GSL-QRNG-REVERSEHALTON"
           "GSL-QRNG-ALLOC"
           "GSL-QRNG-FREE"
           "GSL-QRNG-INIT"
           "GSL-QRNG-GET"
           "GSL-QRNG-NAME"
           "GSL-QRNG-SIZE"
           "GSL-QRNG-STATE"
           "GSL-QRNG-MEMCPY"
           "GSL-QRNG-CLONE"))

(cl:in-package "GSL-QRNG")

;;; (struct gsl-qrng-type)
;;;   Structure describing a type of generator.
(define-alien-type nil
    (struct gsl-qrng-type
            (name (* char))
            (max-dimension unsigned-int)
            (state-size (* (function size-t unsigned-int)))
            (init-state (* (function int (* t) unsigned-int)))
            (get (* (function int (* t) unsigned-int (array double nil))))))

;;; (struct gsl-qrng)
;;;   Structure describing a generator instance of a specified type, with generator-
;;;   specific state info and dimension-specific info.
(define-alien-type nil
    (struct gsl-qrng
            (type (* (struct gsl-qrng-type)))
            (dimension unsigned-int)
            (state-size size-t)
            (state (* t))))

;;; The following quasi-random sequence algorithms are available.
(define-alien-variable gsl-qrng-niederreiter_2 (* (struct gsl-qrng-type)))
(define-alien-variable gsl-qrng-sobol (* (struct gsl-qrng-type)))
(define-alien-variable gsl-qrng-halton (* (struct gsl-qrng-type)))
(define-alien-variable gsl-qrng-reversehalton (* (struct gsl-qrng-type)))

;;; (gsl-qrng-alloc type dimension)
;;;   This function returns a pointer to a newly-created instance of a quasi-random sequence
;;;   generator of type and dimension. If there is insufficient memory to create the generator
;;;   then the function returns a null pointer and the error handler is invoked with an error
;;;   code of GSL_ENOMEM.
(define-alien-routine gsl-qrng-alloc
    (* (struct gsl-qrng))
  (type (* (struct gsl-qrng-type)))
  (dimension unsigned-int))

;;; (gsl-qrng-free q)
;;;   This function frees all the memory associated with the generator q.
(define-alien-routine gsl-qrng-free
    void
  (q (* (struct gsl-qrng))))

;;; (gsl-qrng-init q)
;;;   This function reinitializes the generator q to its starting point. Note that quasi-
;;;   random sequences do not use a seed and always produce the same set of values.
(define-alien-routine gsl-qrng-init
    void
  (q (* (struct gsl-qrng))))

;;; (gsl-qrng-get q x)
;;;   This function stores the next point from the sequence generator q in the array x. The
;;;   space available for x must match the dimension of the generator. The point x will lie
;;;   in the range 0 < x[i] < 1 for each x[i]. An inline version of this function is used when
;;;   HAVE_INLINE is defined.
(define-alien-routine gsl-qrng-get
    int
  (q (* (struct gsl-qrng)))
  (x (array double nil)))

;;; (gsl-qrng-name q)
;;;   This function returns a pointer to the name of the generator.
(define-alien-routine gsl-qrng-name
    (* char)
  (q (* (struct gsl-qrng))))

;;; (gsl-qrng-size q)
;;; (gsl-qrng-state q)
;;;   These functions return a pointer to the state of generator q and its size. You can
;;;   use this information to access the state directly.
(define-alien-routine gsl-qrng-size
    size-t
  (q (* (struct gsl-qrng))))

(define-alien-routine gsl-qrng-state
    (* t)
  (q (* (struct gsl-qrng))))

;;; (gsl-qrng-memcpy dest src)
;;;   This function copies the quasi-random sequence generator src into the pre-existing
;;;   generator dest, making dest into an exact copy of src. The two generators must be
;;;   of the same type.
(define-alien-routine gsl-qrng-memcpy
    int
  (dest (* (struct gsl-qrng))))

;;; (gsl-qrng-clone q)
;;;   This function returns a pointer to a newly created generator which is an exact copy
;;;   of the generator q.
(define-alien-routine gsl-qrng-clone
    (* (struct gsl-qrng))
  (q (* (struct gsl-qrng))))
