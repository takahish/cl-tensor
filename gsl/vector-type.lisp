;;;; gsl/vector-type.lisp
;;;;
;;;; Vectors are defined by a gsl-vector structure which describes a
;;;; slice of a block. Different vectors can be created which point to
;;;; the same block.  A vector slice is a set of equally-spaced
;;;; elements of an area of memory.

;;;; Copyright (C) 2016, 2017 Takahiro Ishikawa
;;;;
;;;; This program is free software: you can redistribute it and/or modif(loay
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


;;; gsl-vector

;; abstract: scl::vector-t, vector-any
(defclass vector-any (scl::vector-t) ())

(defclass vector-double (vector-any) ())

(defclass vector-float (vector-any) ())

(defclass vector-int (vector-any) ())

(defclass vector-uint (vector-any) ())


(defun vector-alloc (n &key (element-type :double))
  "This function creates a vector of length n, returning a pointer to
a newly initialized vector struct. A new block is allocated for the
elements of the vector, and stored in the block component of the
vector struct. The block is owned by the vector, and will be
deallocated when the vector is deallocated."
  (cond
    ((eql element-type :double)
     (make-instance 'vector-double
                    :data (gsl_vector_alloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :float)
     (make-instance 'vector-float
                    :data (gsl_vector_float_alloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :int)
     (make-instance 'vector-int
                    :data (gsl_vector_int_alloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :unsigned-int)
     (make-instance 'vector-uint
                    :data (gsl_vector_uint_alloc n)
                    :size n
                    :stride 1))
    (t (error "unknown element type"))))

(defun vector-calloc (n &key (element-type :double))
  "This function allocates memory for a vector of length n and
initializes all the elements of the vector to zero."
  (cond
    ((eql element-type :double)
     (make-instance 'vector-double
                    :data (gsl_vector_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :float)
     (make-instance 'vector-float
                    :data (gsl_vector_float_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :int)
     (make-instance 'vector-int
                    :data (gsl_vector_int_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :unsigned-int)
     (make-instance 'vector-uint
                    :data (gsl_vector_uint_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    (t (error "unknown element type"))))

(defgeneric vector-free (v &optional result)
  (:documentation
   "This function frees a previously allocated vector v. If the vector
was created using vector-alloc then the block underlying the
vector will also be deallocated. If the vector has been created from
another object then the memory is still owned by that object and will
not be deallocated."))

(defmacro make-vector-free (type func)
  `(defmethod vector-free ((v ,type) &optional (result nil))
     (,func (scl::data v))
     result))

(make-vector-free vector-double gsl_vector_free)

(make-vector-free vector-float gsl_vector_float_free)

(make-vector-free vector-int gsl_vector_int_free)

(make-vector-free vector-uint gsl_vector_uint_free)

(defgeneric vector-set-sequence (v seq &optional n)
  (:documentation
   "This function sets each element of the vector v to each element of
the sequence seq respectively."))

(defmacro make-vector-set-sequence (type sfunc)
  `(defmethod vector-set-sequence ((v ,type) seq &optional (n nil))
     (dotimes (i (if (null n) (scl::size v) n) v)
       (,sfunc (scl::data v) i (elt seq i)))))

(make-vector-set-sequence vector-double gsl_vector_set)

(make-vector-set-sequence vector-float gsl_vector_float_set)

(make-vector-set-sequence vector-int gsl_vector_int_set)

(make-vector-set-sequence vector-uint gsl_vector_uint_set)

(defun make-vector (n &key (initial-element nil)
                        (initial-contents nil)
                        (element-type :double))
  "This function makes a vector of length n, returning a instance to a
newly initialized vector type. A new block is allocated for the
elements of the vector, and stored in the block component of the
vector struct. The block is owned by the vector, and will be
deallocated when the vector is deallocated.
The memory is allocated using vector-calloc, so it can be passed to
function which vector-free."
  (let ((v (vector-calloc n :element-type element-type)))
    (cond ((not (null initial-element))
           (vector-set-all v initial-element))
          ((not (null initial-contents))
           (vector-set-sequence v initial-contents n))
          (t v))))

(defmacro with-vector ((var &rest make-vector-args) &body body)
  `(let ((,var (make-vector ,@make-vector-args)))
     (unwind-protect (progn ,@body)
       (vector-free ,var))))
