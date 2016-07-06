;;; sbcl-gsl/blocks.lisp
;;;
;;; The functions described in sbcl-gsl/vector.lisp and sbcl/matrix.lisp provide
;;; a simple vector and matrix interface to ordinary C arrays. The memory
;;; management of these arrays is implemented using a single underlying type,
;;; known as a block. By writing your functions in terms of vectors and matrices
;;; you can pass a single structure containing both data and dimentions as an
;;; argument without needing additional function parameters. The structures are
;;; compatible with the vector and matrix formats used by BLAS routines.

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

(cl:defpackage "SB-GSL-BLOCK"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL")
  (:export "GSL-BLOCK"
           "GSL-BLOCK-ALLOC"
           "GSL-BLOCK-CALLOC"
           "GSL-BLOCK-FREE"))

(cl:in-package "SB-GSL-BLOCK")

;;; gsl_block_struct
(define-alien-type nil
    (struct gsl-block
            (size size-t)
            (data (* double))))

;;; (gsl-block-alloc n)
;;;   This function allocates memory for a block of n double-precision elements, returning
;;;   a pointer to the block struct. The block is not initialized and so the values of its
;;;   elements are undefined. Use the function gsl-block-calloc if you want to ensure
;;;   that all the elements are initialized to zero.
;;;   A null pointer is returned if insufficient memory is available to create the block.
(define-alien-routine gsl-block-alloc
    (* (struct gsl-block))
  (n size-t))

;;; (gsl-block-calloc n)
;;;   This funciton allocates memory for a block and initialized all the elements of the block
;;;   to zero.
(define-alien-routine gsl-block-calloc
    (* (struct gsl-block))
  (n size-t))

;;; (gsl-block-free b)
;;;   This function frees the memory used by a block b previously allocated with
;;;   gsl-block-alloc or gsl-block-calloc.
(define-alien-routine gsl-block-free
    void
  (b (* (struct gsl-block))))

;;; (test-gsl-block)
;;;   This function do tests.
(defun test-gsl-block ()
  (with-alien ((b (* (struct gsl-block))))
    (setf b (gsl-block-alloc 100))
    (format t "length of block = ~A~%" (slot (deref b) 'size))
    (format t "blcok data object = ~A~%" (slot (deref b) 'data))
    (gsl-block-free b)))
