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

(cl:in-package "GSL")

;;; gsl_block_struct
(defcstruct gsl-block
  (size :unsigned-int)
  (data (:pointer :double)))

;;; gsl_block_float_struct
(defcstruct gsl-block-float
  (size :unsigned-int)
  (data (:pointer :float)))
