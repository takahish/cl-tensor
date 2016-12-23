;;;; cl-sct/pkg.lisp

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

(cl:defpackage "SCT"
  (:use "CL")
  (:export ;; cl-sct/utils.lisp
           "LAST1"
           "FLATTEN"
           ;; cl-sct/simple-vector.lisp
           "MAKE-SIMPLE-VECTOR"
           "SIMPLE-VECTOR-GET"
           "SIMPLE-VECTOR-SET"
           "SIMPLE-VECTOR-SET-ALL"
           "SIMPLE-VECTOR-SET-ZERO"
           "SIMPLE-VECTOR-SET-BASIS"
           "SIMPLE-VECTOR-SUBVECTOR"
           "SIMPLE-VECTOR-COPY"
           "SIMPLE-VECTOR-SWAP-ELEMENTS"
           "SIMPLE-VECTOR-REVERSE"
           "SIMPLE-VECTOR-ADD"
           "SIMPLE-VECTOR-SUB"
           "SIMPLE-VECTOR-MUL"
           "SIMPLE-VECTOR-DIV"
           "SIMPLE-VECTOR-SCALE"
           "SIMPLE-VECTOR-ADD-CONSTANT"
           "SIMPLE-VECTOR-MAX"
           "SIMPLE-VECTOR-MIN"
           "SIMPLE-VECTOR-MINMAX"
           "SIMPLE-VECTOR-MAX-INDEX"
           "SIMPLE-VECTOR-MIN-INDEX"
           "SIMPLE-VECTOR-MINMAX-INDEX"
           "SIMPLE-VECTOR-ISNULL"
           "SIMPLE-VECTOR-ISPOS"
           "SIMPLE-VECTOR-ISNEG"
           "SIMPLE-VECTOR-ISNONNEG"
           "SIMPLE-VECTOR-EQUAL"
           "SIMPLE-VECTOR-READ"
           "SIMPLE-VECTOR-WRITE"
           "SIMPLE-VECTOR-MAP"
           "SIMPLE-VECTOR-REDUCE"
           ;; cl-sct/simple-matrix.lisp
           "MAKE-SIMPLE-MATRIX"
           "SIMPLE-MATRIX-GET"
           "SIMPLE-MATRIX-SET"))
