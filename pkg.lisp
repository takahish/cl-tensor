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
  (:export ;; cl-sct/util.lisp
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
           "SIMPLE-VECTOR-VIEW-ARRAY"
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
           "SIMPLE-MATRIX-SET"
           "SIMPLE-MATRIX-SET-ALL"
           "SIMPLE-MATRIX-SET-ZERO"
           "SIMPLE-MATRIX-SET-IDENTITY"
           "SIMPLE-MATRIX-SUBMATRIX"
           "SIMPLE-MATRIX-VIEW-VECTOR"
           "SIMPLE-MATRIX-ROW"
           "SIMPLE-MATRIX-COLUMN"
           "SIMPLE-MATRIX-SUBROW"
           "SIMPLE-MATRIX-SUBCOLUMN"
           "SIMPLE-MATRIX-DIAGONAL"
           "SIMPLE-MATRIX-SUBDIAGONAL"
           "SIMPLE-MATRIX-SUPERDIAGONAL"
           "SIMPLE-MATRIX-COPY"
           "SIMPLE-MATRIX-GET-ROW"
           "SIMPLE-MATRIX-SET-ROW"
           "SIMPLE-MATRIX-GET-COL"
           "SIMPLE-MATRIX-SET-COL"
           "SIMPLE-MATRIX-SWAP-ROWS"
           "SIMPLE-MATRIX-SWAP-COLUMNS"
           "SIMPLE-MATRIX-TRANSPOSE"
           "SIMPLE-MATRIX-ADD"
           "SIMPLE-MATRIX-SUB"
           "SIMPLE-MATRIX-MUL-ELEMENTS"
           "SIMPLE-MATRIX-DIV-ELEMENTS"
           "SIMPLE-MATRIX-SCALE"
           "SIMPLE-MATRIX-ADD-CONSTANT"
           "SIMPLE-MATRIX-MAX"
           "SIMPLE-MATRIX-MIN"
           "SIMPLE-MATRIX-MINMAX"
           "SIMPLE-MATRIX-MAX-INDEX"
           "SIMPLE-MATRIX-MIN-INDEX"
           "SIMPLE-MATRIX-MINMAX-INDEX"
           "SIMPLE-MATRIX-ISNULL"
           "SIMPLE-MATRIX-ISPOS"
           "SIMPLE-MATRIX-ISNEG"
           "SIMPLE-MATRIX-ISNONNEG"
           "SIMPLE-MATRIX-EQUAL"
           "SIMPLE-MATRIX-READ"
           "SIMPLE-MATRIX-WRITE"))
