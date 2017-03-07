;;;; gsl/matrix.lisp
;;;;
;;;; Matrices are defined by a gsl-matrix structure which describes a
;;;; generalized slice of a block. Like a vector it represents a set
;;;; of elements in an area of memory, but uses two indices instead of
;;;; one.

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


;;; gsl-matrix

;; abstract: scl::matrix-t, gsl-matrix-any
(defclass gsl-matrix-any (scl::matrix-t) ())

(defclass gsl-matrix-double (gsl-matrix-any) ())

(defclass gsl-matrix-float (gsl-matrix-any) ())

(defclass gsl-matrix-int (gsl-matrix-any) ())

(defclass gsl-matrix-uint (gsl-matrix-any) ())


(defun matrix-alloc (n1 n2 &key (element-type :double))
  "This function create a matrix of size n1 rows by n2 columns,
returning a pointer to a newly initialized matrix struct. A new block
is allocated for the elements of the matrix, and stored in the block
component of the matrix struct. The bclok is owned by the matrix, and
will be deallocated when the matrix is deallocated."
  (cond ((eql element-type :double)
         (make-instance 'gsl-matrix-double
                        :data (gsl_matrix_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        ((eql element-type :float)
         (make-instance 'gsl-matrix-float
                        :data (gsl_matrix_float_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        ((eql element-type :int)
         (make-instance 'gsl-matrix-int
                        :data (gsl_matrix_int_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        ((eql element-type :unsigned-int)
         (make-instance 'gsl-matrix-uint
                        :data (gsl_matrix_uint_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        (t (error "unknown element type"))))

(defun matrix-calloc (n1 n2 &key (element-type :double))
  "This function allocates memory for a matrix of size n1 rows by n2
columns and initializes all the elements of the matrix to zero."
  (cond ((eql element-type :double)
         (make-instance 'gsl-matrix-double
                        :data (gsl_matrix_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        ((eql element-type :float)
         (make-instance 'gsl-matrix-float
                        :data (gsl_matrix_float_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        ((eql element-type :int)
         (make-instance 'gsl-matrix-int
                        :data (gsl_matrix_int_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        ((eql element-type :unsigned-int)
         (make-instance 'gsl-matrix-uint
                        :data (gsl_matrix_uint_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2
                        :owner t))
        (t (error "unknown element type"))))

(defgeneric matrix-free (m &optional result)
  (:documentation
   "This function frees a previously allocated matrix m. If the matrix
was created using gsl_matrix_alloc then the block underlying the
matrix will also be deallocated."))

(defmacro make-matrix-free (type func)
  `(defmethod matrix-free ((m ,type) &optional (result nil))
     (,func (scl::data m))
     result))

(make-matrix-free gsl-matrix-double gsl_matrix_free)

(make-matrix-free gsl-matrix-float gsl_matrix_float_free)

(make-matrix-free gsl-matrix-int gsl_matrix_int_free)

(make-matrix-free gsl-matrix-uint gsl_matrix_uint_free)

(defgeneric matrix-set-sequence (m seq &optional n1 n2)
  (:documentation
   "This function sets each element of the matrix m to each element of
the sequence seq respectively."))

(defmacro make-matrix-set-sequence (type func)
  `(defmethod matrix-set-sequence ((m ,type) seq
                                   &optional (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (scl::size1 m) n1))
           (s2 (if (null n2) (scl::size2 m) n2))
           (idx 0))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (,func (scl::data m) i j (elt seq idx))
           (setf idx (1+ idx)))))))

(make-matrix-set-sequence gsl-matrix-double gsl_matrix_set)

(make-matrix-set-sequence gsl-matrix-float gsl_matrix_float_set)

(make-matrix-set-sequence gsl-matrix-int gsl_matrix_int_set)

(make-matrix-set-sequence gsl-matrix-uint gsl_matrix_uint_set)

(defgeneric matrix-set-2darray (m 2darray &optional n1 n2)
  (:documentation
   "This function sets each element of the matrix m to each element of
the 2 dimensions array respectively."))

(defmacro make-matrix-set-2darray (type func)
  `(defmethod matrix-set-2darray ((m ,type) 2darray
                                  &optional (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (scl::size1 m) n1))
           (s2 (if (null n2) (scl::size2 m) n2)))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (,func (scl::data m) i j (aref 2darray i j)))))))

(make-matrix-set-2darray gsl-matrix-double gsl_matrix_set)

(make-matrix-set-2darray gsl-matrix-float gsl_matrix_float_set)

(make-matrix-set-2darray gsl-matrix-int gsl_matrix_int_set)

(make-matrix-set-2darray gsl-matrix-uint gsl_matrix_uint_set)

;;; Allocate an aline of (struct gsl-matrix) in foeign heap, and return a pointer to it.
(defun make-matrix (n1 n2 &key (initial-element nil)
                            (initial-contents nil)
                            (element-type :double))
  "This function creates a matrix of size n1 rows by n2 columns,
returning a pointer to a newly initialized matrix struct. A new block
is allocated for the elements of the matrix, and stored in the block
component of the matrix struct. The block is owned by the matrix, and
will be deallocated when the matrix is deallocated.  The memory is
allocated using gsl-matrix-alloc, so it can be passed to foreign
functions which gsl-matrix-free, or released using free-alien."
  (let ((m (matrix-calloc n1 n2 :element-type element-type)))
    (cond ((not (null initial-element))
           (matrix-set-all m initial-element))
          ((not (null initial-contents))
           (cond ((consp initial-contents)
                  (matrix-set-sequence m (scl::flatten initial-contents) n1 n2))
                 ((and (arrayp initial-contents)
                       (= (length (array-dimensions initial-contents)) 1))
                  (matrix-set-sequence m initial-contents n1 n2))
                 ((and (arrayp initial-contents)
                       (= (length (array-dimensions initial-contents)) 2))
                  (matrix-set-2darray m initial-contents n1 n2))))
          (t m))))
