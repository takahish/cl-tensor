;;;; pkg.lisp

;;;; Copyright (C) 2016, 2017 Takahiro Ishikawa
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(cl:defpackage "EIGEN"
  (:use "CL")
  (:export ;; util.lisp
           "LAST1"
           "FLATTEN"
           "RANGE"
           "WHILE"
           "WITH-GENSYMS"
           ;; vector-type.lisp
           "MAKE-VECTOR"
           "SHARED-VECTOR" ; accessor of vector-view
           ;; vector.lisp
           "VECTOR-COERCE"
           "VECTOR-GET"
           "VECTOR-SET"
           "VECTOR-SET-ALL"
           "VECTOR-SET-ZERO"
           "VECTOR-SET-BASIS"
           "VECTOR-SUBVECTOR"
           "VECTOR-VIEW-ARRAY"
           "VECTOR-COPY"
           "VECTOR-SWAP-ELEMENTS"
           "VECTOR-REVERSE"
           "VECTOR-ADD"
           "VECTOR-SUB"
           "VECTOR-MUL"
           "VECTOR-DIV"
           "VECTOR-SCALE"
           "VECTOR-ADD-CONSTANT"
           "VECTOR-MAX"
           "VECTOR-MIN"
           "VECTOR-MINMAX"
           "VECTOR-MAX-INDEX"
           "VECTOR-MIN-INDEX"
           "VECTOR-MINMAX-INDEX"
           "VECTOR-ISNULL"
           "VECTOR-ISPOS"
           "VECTOR-ISNEG"
           "VECTOR-ISNONNEG"
           "VECTOR-EQUAL"
           "VECTOR-READ"
           "VECTOR-WRITE"
           "VECTOR-MAP"
           "VECTOR-REDUCE"
           "VECTOR-COUNT-IF"
           "VECTOR-REMOVE-IF"
           ;; matrix-type.lisp
           "MAKE-MATRIX"
           "SHARED-MATRIX" ; accessor of matrix-view
           ;; matrix.lisp
           "MATRIX-COERCE"
           "MATRIX-GET"
           "MATRIX-SET"
           "MATRIX-SET-ALL"
           "MATRIX-SET-ZERO"
           "MATRIX-SET-IDENTITY"
           "MATRIX-SUBMATRIX"
           "MATRIX-VIEW-VECTOR"
           "MATRIX-VIEW-ARRAY"
           "MATRIX-ROW"
           "MATRIX-COLUMN"
           "MATRIX-SUBROW"
           "MATRIX-SUBCOLUMN"
           "MATRIX-DIAGONAL"
           "MATRIX-SUBDIAGONAL"
           "MATRIX-SUPERDIAGONAL"
           "MATRIX-COPY"
           "MATRIX-GET-ROW"
           "MATRIX-SET-ROW"
           "MATRIX-GET-COL"
           "MATRIX-SET-COL"
           "MATRIX-SWAP-ROWS"
           "MATRIX-SWAP-COLUMNS"
           "MATRIX-TRANSPOSE"
           "MATRIX-ADD"
           "MATRIX-SUB"
           "MATRIX-MUL-ELEMENTS"
           "MATRIX-DIV-ELEMENTS"
           "MATRIX-SCALE"
           "MATRIX-ADD-CONSTANT"
           "MATRIX-MAX"
           "MATRIX-MIN"
           "MATRIX-MINMAX"
           "MATRIX-MAX-INDEX"
           "MATRIX-MIN-INDEX"
           "MATRIX-MINMAX-INDEX"
           "MATRIX-ISNULL"
           "MATRIX-ISPOS"
           "MATRIX-ISNEG"
           "MATRIX-ISNONNEG"
           "MATRIX-EQUAL"
           "MATRIX-READ"
           "MATRIX-WRITE"
           ;; blas/blas.lisp
           "BLAS-DOT"
           "BLAS-NRM2"
           "BLAS-ASUM"
           "BLAS-IAMAX"
           "BLAS-SWAP"
           "BLAS-COPY"
           "BLAS-AXPY"
           "BLAS-SCAL"
           "BLAS-ROTG"
           "BLAS-ROT"
           "BLAS-ROTMG"
           "BLAS-ROTM"
           "BLAS-GEMV"
           "BLAS-TRMV"
           "BLAS-SYMV"
           "BLAS-GER"
           "BLAS-SYR"
           "BLAS-SYR2"
           "BLAS-GEMM"
           "BLAS-SYMM"
           "BLAS-TRMM"
           "BLAS-TRSM"
           "BLAS-SYRK"
           "BLAS-SYR2K"))

(cl:in-package "EIGEN")

(cffi:define-foreign-library libcblas
  (:darwin "libcblas.dylib")
  (:unix "libcblas.so")
  (t (:default "libcblas")))

(cffi:use-foreign-library libcblas)
