;;;; cl-scl/gsl/sort.lisp
;;;;
;;;; This file describes functions for sorting data, both directly and
;;;; indirectly (using an index). All the functions use the heapsort
;;;; algorithm.  Heapsort is an O(N log N) algorithm which operates
;;;; in-place and dose not require any additional storage. It also
;;;; provides consistent performance, the running time for its
;;;; worst-case (ordered data) being not significantly longer than the
;;;; average and best cases. Note that the heapsort algorithm dose not
;;;; preserve the relative ordering of equal elements - it is an
;;;; unstable sort. However the resulting order of equal elements will
;;;; be consistent across different platforms when using these
;;;; functions.

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


;; functions

(defmacro make-compare-func (name element-type &body body)
  `(cffi:defcallback ,name :int ((a :pointer) (b :pointer))
     (let ((x (cffi:mem-ref a ,element-type))
           (y (cffi:mem-ref b ,element-type)))
       ,@body)))

(make-compare-func <-double :double
  (cond ((> x y) 1)
        ((< x y) -1)
        (t 0)))

(make-compare-func >-double :double
  (cond ((< x y) 1)
        ((> x y) -1)
        (t 0)))

(make-compare-func <-float :float
  (cond ((> x y) 1)
        ((< x y) -1)
        (t 0)))

(make-compare-func >-float :float
  (cond ((< x y) 1)
        ((> x y) -1)
        (t 0)))

(make-compare-func <-int :int
  (cond ((> x y) 1)
        ((< x y) -1)
        (t 0)))

(make-compare-func >-int :int
  (cond ((< x y) 1)
        ((> x y) -1)
        (t 0)))

(make-compare-func <-uint :unsigned-int
  (cond ((> x y) 1)
        ((< x y) -1)
        (t 0)))

(make-compare-func >-uint :unsigned-int
  (cond ((< x y) 1)
        ((> x y) -1)
        (t 0)))

(defgeneric sort-asc (v &optional n)
  (:documentation
   "This function sorts the n elements of the vector into ascending
order."))

(defmacro make-sort-asc-scl-vector (class element-type func)
  `(defmethod sort-asc ((v ,class) &optional (n nil))
     (let ((size (if (null n) (scl::size v) n)))
       (cffi:with-pointer-to-vector-data (data (scl::data v))
         (gsl_heapsort data size
                       (cffi:foreign-type-size ,element-type)
                       (cffi:callback ,func))
         v))))

(make-sort-asc-scl-vector scl::vector-double :double <-double)

(make-sort-asc-scl-vector scl::vector-float :float <-float)

(make-sort-asc-scl-vector scl::vector-int :int <-int)

(make-sort-asc-scl-vector scl::vector-uint :unsigned-int <-uint)

(defmacro make-sort-asc-gsl-vector (class element-type func)
  `(defmethod sort-asc ((v ,class) &optional (n nil))
     (let ((size (if (null n) (scl::size v) n)))
       (gsl_heapsort (vector-ptr v 0) size
                     (cffi:foreign-type-size ,element-type)
                     (cffi:callback ,func))
       v)))

(make-sort-asc-gsl-vector gsl-vector-double :double <-double)

(make-sort-asc-gsl-vector gsl-vector-float :float <-float)

(make-sort-asc-gsl-vector gsl-vector-int :int <-int)

(make-sort-asc-gsl-vector gsl-vector-uint :unsigned-int <-uint)

(defgeneric sort-desc (v &optional n)
  (:documentation
   "This function sorts the n elements of the vector into descending
order."))

(defmacro make-sort-desc-scl-vector (class element-type func)
  `(defmethod sort-desc ((v ,class) &optional (n nil))
     (let ((size (if (null n) (scl::size v) n)))
       (cffi:with-pointer-to-vector-data (data (scl::data v))
         (gsl_heapsort data size
                       (cffi:foreign-type-size ,element-type)
                       (cffi:callback ,func))
         v))))

(make-sort-desc-scl-vector scl::vector-double :double >-double)

(make-sort-desc-scl-vector scl::vector-float :float >-float)

(make-sort-desc-scl-vector scl::vector-int :int >-int)

(make-sort-desc-scl-vector scl::vector-uint :unsigned-int >-uint)

(defmacro make-sort-desc-gsl-vector (class element-type func)
  `(defmethod sort-desc ((v ,class) &optional (n nil))
     (let ((size (if (null n) (scl::size v) n)))
       (gsl_heapsort (vector-ptr v 0) size
                     (cffi:foreign-type-size ,element-type)
                     (cffi:callback ,func))
       v)))

(make-sort-desc-gsl-vector gsl-vector-double :double >-double)

(make-sort-desc-gsl-vector gsl-vector-float :float >-float)

(make-sort-desc-gsl-vector gsl-vector-int :int >-int)

(make-sort-desc-gsl-vector gsl-vector-uint :unsigned-int >-uint)
