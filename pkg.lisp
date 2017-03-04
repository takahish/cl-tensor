;;;; cl-scl/pkg.lisp

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

(cl:defpackage "SCL"
  (:use "CL")
  (:export ;; cl-scl/util.lisp
           "LAST1"
           "FLATTEN"
           "WHILE"
           ;; cl-scl/vector.lisp
           "MAKE-VECTOR"
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
           ;; cl-scl/matrix.lisp
           "MAKE-MATRIX"
           "MATRIX-COERCE"
           "MATRIX-GET"
           "MATRIX-SET"
           "MATRIX-SET-ALL"
           "MATRIX-SET-ZERO"
           "MATRIX-SET-IDENTITY"
           "MATRIX-SUBMATRIX"
           "MATRIX-VIEW-VECTOR"
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
           ;; cl-scl/data-frame.lisp
           "MAKE-DATA-FRAME"
           "DATA-FRAME-SET-NAMES"
           "DATA-FRAME-SET-INDEX"
           "DATA-FRAME-ROW"
           "DATA-FRAME-COLUMN"
           "DATA-FRAME-GET-ROW"
           "DATA-FRAME-SET-ROW"
           "DATA-FRAME-GET-COL"
           "DATA-FRAME-SET-COL"
           "DATA-FRAME-READ-CSV"))
