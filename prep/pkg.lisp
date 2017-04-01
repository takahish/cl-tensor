;;;; scl/pkg.lisp

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

(cl:defpackage "PREP"
  (:use "CL")
  (:export ;; scl/data-frame-type.lisp
           "MAKE-DATA-FRAME"
           ;; scl/data-frame.lisp
           "DATA-FRAME-SET-NAMES"
           "DATA-FRAME-SET-INDEX"
           "DATA-FRAME-ROW"
           "DATA-FRAME-COLUMN"
           "DATA-FRAME-GET-ROW"
           "DATA-FRAME-SET-ROW"
           "DATA-FRAME-GET-COL"
           "DATA-FRAME-SET-COL"
           "DATA-FRAME-READ-TSV"))
