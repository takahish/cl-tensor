;;;; cl-scl/gsl/vector.lisp
;;;;
;;;; Vectors are defined by a gsl-vector structure which describes a
;;;; slice of a block. Different vectors can be created which point to
;;;; the same block.  A vector slice is a set of equally-spaced
;;;; elements of an area of memory.

;;;; Copyright (C) 2016 Takahiro Ishikawa
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

;; abstract: scl::vector-t, gsl-vector-any
(defclass gsl-vector-any (scl::vector-t) ())

(defclass gsl-vector-double (gsl-vector-any) ())

(defclass gsl-vector-float (gsl-vector-any) ())

(defclass gsl-vector-int (gsl-vector-any) ())

(defclass gsl-vector-uint (gsl-vector-any) ())


;;; functions

(defun vector-alloc (n &key (element-type :double))
  "This function creates a vector of length n, returning a pointer to
a newly initialized vector struct. A new block is allocated for the
elements of the vector, and stored in the block component of the
vector struct. The block is owned by the vector, and will be
deallocated when the vector is deallocated."
  (cond
    ((eql element-type :double)
     (make-instance 'gsl-vector-double
                    :data (gsl_vector_alloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :float)
     (make-instance 'gsl-vector-float
                    :data (gsl_vector_float_alloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :int)
     (make-instance 'gsl-vector-int
                    :data (gsl_vector_int_alloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :unsigned-int)
     (make-instance 'gsl-vector-uint
                    :data (gsl_vector_uint_alloc n)
                    :size n
                    :stride 1))
    (t (error "unknown element type"))))

(defun vector-calloc (n &key (element-type :double))
  "This function allocates memory for a vector of length n and
initializes all the elements of the vector to zero."
  (cond
    ((eql element-type :double)
     (make-instance 'gsl-vector-double
                    :data (gsl_vector_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :float)
     (make-instance 'gsl-vector-float
                    :data (gsl_vector_float_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :int)
     (make-instance 'gsl-vector-int
                    :data (gsl_vector_int_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :unsigned-int)
     (make-instance 'gsl-vector-uint
                    :data (gsl_vector_uint_calloc n)
                    :size n
                    :stride 1
                    :owner t))
    (t (error "unknown element type"))))

(defgeneric vector-free (v &optional result)
  (:documentation
   "This function frees a previously allocated vector v. If the vector
was created using gsl-vector-alloc then the block underlying the
vector will also be deallocated. If the vector has been created from
another object then the memory is still owned by that object and will
not be deallocated."))

(defmacro make-vector-free (type func)
  `(defmethod vector-free ((v ,type) &optional (result nil))
     (,func (scl::data v))
     result))

(make-vector-free gsl-vector-double gsl_vector_free)

(make-vector-free gsl-vector-float gsl_vector_float_free)

(make-vector-free gsl-vector-int gsl_vector_int_free)

(make-vector-free gsl-vector-uint gsl_vector_uint_free)

(defgeneric vector-get (v i)
  (:documentation
   "This function retruns the i-th element of a vector v. If i lies
outside the allowed range of 0 to n - 1 then the error handler is
invoked and 0 is returned."))

(defmacro make-vector-get (type func)
  `(defmethod vector-get ((v ,type) i)
     (,func (scl::data v) i)))

(make-vector-get gsl-vector-double gsl_vector_get)

(make-vector-get gsl-vector-float gsl_vector_float_get)

(make-vector-get gsl-vector-int gsl_vector_int_get)

(make-vector-get gsl-vector-uint gsl_vector_uint_get)

(defgeneric vector-set (v i x)
  (:documentation
   "This function sets the value of the i-th element of a vector v to
x. If i lies outside the allowed range of 0 to n - 1 then the error
handler is invoked."))

(defmacro make-vector-set (type func)
  `(defmethod vector-set ((v ,type) i x)
     (,func (scl::data v) i x)
     v))

(make-vector-set gsl-vector-double gsl_vector_set)

(make-vector-set gsl-vector-float gsl_vector_float_set)

(make-vector-set gsl-vector-int gsl_vector_int_set)

(make-vector-set gsl-vector-uint gsl_vector_uint_set)

(defgeneric vector-ptr (v i)
  (:documentation
   "This function return a pointer to the i-th element of a vector
v. If i lies outside the allowed range of 0 to n - 1 then the error
handler is invoked and a null pointer is returned."))

(defmacro make-vector-ptr (type func)
  `(defmethod vector-ptr ((v ,type) i)
     (,func (scl::data v) i)))

(make-vector-ptr gsl-vector-double gsl_vector_ptr)

(make-vector-ptr gsl-vector-float gsl_vector_float_ptr)

(make-vector-ptr gsl-vector-int gsl_vector_int_ptr)

(make-vector-ptr gsl-vector-uint gsl_vector_uint_ptr)

(defgeneric vector-set-all (v x)
  (:documentation
   "This function sets all the elements of the vector v to the value
x."))

(defmacro make-vector-set-all (type func)
  `(defmethod vector-set-all ((v ,type) x)
     (,func (scl::data v) x)
     v))

(make-vector-set-all gsl-vector-double gsl_vector_set_all)

(make-vector-set-all gsl-vector-float gsl_vector_float_set_all)

(make-vector-set-all gsl-vector-int gsl_vector_int_set_all)

(make-vector-set-all gsl-vector-uint gsl_vector_uint_set_all)

(defgeneric vector-set-zero (v)
  (:documentation
   "This function sets all the elements of the vector v to zero."))

(defmacro make-vector-set-zero (type func)
  `(defmethod vector-set-zero ((v ,type))
     (,func (scl::data v))
     v))

(make-vector-set-zero gsl-vector-double gsl_vector_set_zero)

(make-vector-set-zero gsl-vector-float gsl_vector_float_set_zero)

(make-vector-set-zero gsl-vector-int gsl_vector_int_set_zero)

(make-vector-set-zero gsl-vector-uint gsl_vector_uint_set_zero)

(defgeneric vector-set-basis (v i)
  (:documentation
   "This function makes a basis vector by setting all the elements of
the vector v to zero except for the i-th element which is set to
one."))

(defmacro make-vector-set-basis (type func)
  `(defmethod vector-set-basis ((v ,type) i)
     (,func (scl::data v) i)
     v))

(make-vector-set-basis gsl-vector-double gsl_vector_set_basis)

(make-vector-set-basis gsl-vector-float gsl_vector_float_set_basis)

(make-vector-set-basis gsl-vector-int gsl_vector_int_set_basis)

(make-vector-set-basis gsl-vector-uint gsl_vector_uint_set_basis)

(defgeneric vector-memcpy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest. The two vectors must have the same length."))

(defmacro make-vector-memcpy (type func)
  `(defmethod vector-memcpy ((dest ,type) (src ,type))
     (,func (scl::data dest) (scl::data src))
     dest))

(make-vector-memcpy gsl-vector-double gsl_vector_memcpy)

(make-vector-memcpy gsl-vector-float gsl_vector_float_memcpy)

(make-vector-memcpy gsl-vector-int gsl_vector_int_memcpy)

(make-vector-memcpy gsl-vector-uint gsl_vector_uint_memcpy)

(defgeneric vector-swap (v w)
  (:documentation
   "This function exhanges the elements of the vectors v and w by
copying. The two vectors must have the same length."))

(defmacro make-vector-swap (type func)
  `(defmethod vector-swap ((v ,type) (w ,type))
     (,func (scl::data v) (scl::data w))
     (values v w)))

(make-vector-swap gsl-vector-double gsl_vector_swap)

(make-vector-swap gsl-vector-float gsl_vector_float_swap)

(make-vector-swap gsl-vector-int gsl_vector_int_swap)

(make-vector-swap gsl-vector-uint gsl_vector_uint_swap)

(defgeneric vector-swap-elements (v i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the vector v
in-place."))

(defmacro make-vector-swap-elements (type func)
  `(defmethod vector-swap-elements ((v ,type) i j)
     (,func (scl::data v) i j)
     v))

(make-vector-swap-elements gsl-vector-double gsl_vector_swap_elements)

(make-vector-swap-elements gsl-vector-float gsl_vector_float_swap_elements)

(make-vector-swap-elements gsl-vector-int gsl_vector_int_swap_elements)

(make-vector-swap-elements gsl-vector-uint gsl_vector_uint_swap_elements)

(defgeneric vector-reverse (v)
  (:documentation
   "This function reverses the order of the elements of the vector v."))

(defmacro make-vector-reverse (type func)
  `(defmethod vector-reverse ((v ,type))
     (,func (scl::data v))
     v))

(make-vector-reverse gsl-vector-double gsl_vector_reverse)

(make-vector-reverse gsl-vector-float gsl_vector_float_reverse)

(make-vector-reverse gsl-vector-int gsl_vector_int_reverse)

(make-vector-reverse gsl-vector-uint gsl_vector_uint_reverse)

(defgeneric vector-add (a b)
  (:documentation
   "This function adds the elements of vector b to the elements of
vector a. The result a_i <- a_i + b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-add (type func)
  `(defmethod vector-add ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-add gsl-vector-double gsl_vector_add)

(make-vector-add gsl-vector-float gsl_vector_float_add)

(make-vector-add gsl-vector-int gsl_vector_int_add)

(make-vector-add gsl-vector-uint gsl_vector_uint_add)

(defgeneric vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements
of vector a. The result a_i <- a_i - b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-sub (type func)
  `(defmethod vector-sub ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-sub gsl-vector-double gsl_vector_sub)

(make-vector-sub gsl-vector-float gsl_vector_float_sub)

(make-vector-sub gsl-vector-int gsl_vector_int_sub)

(make-vector-sub gsl-vector-uint gsl_vector_uint_sub)

(defgeneric vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements
of vector b. The result a_i <- a_i * b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-mul (type func)
  `(defmethod vector-mul ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-mul gsl-vector-double gsl_vector_mul)

(make-vector-mul gsl-vector-float gsl_vector_float_mul)

(make-vector-mul gsl-vector-int gsl_vector_int_mul)

(make-vector-mul gsl-vector-uint gsl_vector_uint_mul)

(defgeneric vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of
vector b. The result a_i <- a_i / b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-div (type func)
  `(defmethod vector-div ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-div gsl-vector-double gsl_vector_div)

(make-vector-div gsl-vector-float gsl_vector_float_div)

(make-vector-div gsl-vector-int gsl_vector_int_div)

(make-vector-div gsl-vector-uint gsl_vector_uint_div)

(defgeneric vector-scale (a x)
  (:documentation
   "This function multiplies the elements of vector a by the constant
factor x. The result a_i <- x * a_i is stored in a."))

(defmacro make-vector-scale (type func)
  `(defmethod vector-scale ((a ,type) x)
     (,func (scl::data a) x)
     a))

(make-vector-scale gsl-vector-double gsl_vector_scale)

(make-vector-scale gsl-vector-float gsl_vector_float_scale)

(make-vector-scale gsl-vector-int gsl_vector_int_scale)

(make-vector-scale gsl-vector-uint gsl_vector_uint_scale)

(defgeneric vector-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
vector a. The result a_i <- a_i + x is stored in a."))

(defmacro make-vector-add-constant (type func)
  `(defmethod vector-add-constant ((a ,type) x)
     (,func (scl::data a) x)
     a))

(make-vector-add-constant gsl-vector-double gsl_vector_add_constant)

(make-vector-add-constant gsl-vector-float gsl_vector_float_add_constant)

(make-vector-add-constant gsl-vector-int gsl_vector_int_add_constant)

(make-vector-add-constant gsl-vector-uint gsl_vector_uint_add_constant)

(defgeneric vector-max (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmacro make-vector-max (type func)
  `(defmethod vector-max ((v ,type))
     (,func (scl::data v))))

(make-vector-max gsl-vector-double gsl_vector_max)

(make-vector-max gsl-vector-float gsl_vector_float_max)

(make-vector-max gsl-vector-int gsl_vector_int_max)

(make-vector-max gsl-vector-uint gsl_vector_uint_max)

(defgeneric vector-min (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmacro make-vector-min (type func)
  `(defmethod vector-min ((v ,type))
     (,func (scl::data v))))

(make-vector-min gsl-vector-double gsl_vector_min)

(make-vector-min gsl-vector-float gsl_vector_float_min)

(make-vector-min gsl-vector-int gsl_vector_int_min)

(make-vector-min gsl-vector-uint gsl_vector_uint_min)

(defgeneric vector-minmax (v)
  (:documentation
   "This function returns the minimum and maximum values in the vector v."))

(defmacro make-vector-minmax (type element-type func)
  `(defmethod vector-minmax ((v ,type))
     (cffi:with-foreign-objects ((min-out ,element-type)
                                 (max-out ,element-type))
       (,func (scl::data v) min-out max-out)
       (values (cffi:mem-ref min-out ,element-type)
               (cffi:mem-ref max-out ,element-type)))))

(make-vector-minmax gsl-vector-double :double gsl_vector_minmax)

(make-vector-minmax gsl-vector-float :float gsl_vector_float_minmax)

(make-vector-minmax gsl-vector-int :int gsl_vector_int_minmax)

(make-vector-minmax gsl-vector-uint :unsigned-int gsl_vector_uint_minmax)

(defgeneric vector-max-index (v)
  (:documentation
   "This function returns the index of the maximum value in the vector
v. When there are several equal maximum elements then the lowest index
is returned."))

(defmacro make-vector-max-index (type func)
  `(defmethod vector-max-index ((v ,type))
     (,func (scl::data v))))

(make-vector-max-index gsl-vector-double gsl_vector_max_index)

(make-vector-max-index gsl-vector-float gsl_vector_float_max_index)

(make-vector-max-index gsl-vector-int gsl_vector_int_max_index)

(make-vector-max-index gsl-vector-uint gsl_vector_uint_max_index)

(defgeneric vector-min-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the vector v, storing them in imin and imax. When there are
several equal minimum or maximum elements then the lowest indices are
returned."))

(defmacro make-vector-min-index (type func)
  `(defmethod vector-min-index ((v ,type))
     (,func (scl::data v))))

(make-vector-min-index gsl-vector-double gsl_vector_min_index)

(make-vector-min-index gsl-vector-float gsl_vector_float_min_index)

(make-vector-min-index gsl-vector-int gsl_vector_int_min_index)

(make-vector-min-index gsl-vector-uint gsl_vector_uint_min_index)

(defgeneric vector-minmax-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the vector v."))

(defmacro make-vector-minmax-index (type func)
  `(defmethod vector-minmax-index ((v ,type))
     (cffi:with-foreign-objects ((imin :unsigned-int)
                                 (imax :unsigned-int))
       (,func (scl::data v) imin imax)
       (values (cffi:mem-ref imin :unsigned-int)
               (cffi:mem-ref imax :unsigned-int)))))

(make-vector-minmax-index gsl-vector-double gsl_vector_minmax_index)

(make-vector-minmax-index gsl-vector-float gsl_vector_float_minmax_index)

(make-vector-minmax-index gsl-vector-int gsl_vector_int_minmax_index)

(make-vector-minmax-index gsl-vector-uint gsl_vector_uint_minmax_index)

(defgeneric vector-isnull (v)
  (:documentation
   "This function return t if all the elements of the vector v are
zero, and nil otherwise."))

(defmacro make-vector-isnull (type func)
  `(defmethod vector-isnull ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-isnull gsl-vector-double gsl_vector_isnull)

(make-vector-isnull gsl-vector-float gsl_vector_float_isnull)

(make-vector-isnull gsl-vector-int gsl_vector_int_isnull)

(make-vector-isnull gsl-vector-uint gsl_vector_uint_isnull)

(defgeneric vector-ispos (v)
  (:documentation
   "This function return t if all the elements of the vector v are
strictly positive, and nil otherwise."))

(defmacro make-vector-ispos (type func)
  `(defmethod vector-ispos ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-ispos gsl-vector-double gsl_vector_ispos)

(make-vector-ispos gsl-vector-float gsl_vector_float_ispos)

(make-vector-ispos gsl-vector-int gsl_vector_int_ispos)

(make-vector-ispos gsl-vector-uint gsl_vector_uint_ispos)

(defgeneric vector-isneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are
strictly negative, and nil otherwise."))

(defmacro make-vector-isneg (type func)
  `(defmethod vector-isneg ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-isneg gsl-vector-double gsl_vector_isneg)

(make-vector-isneg gsl-vector-float gsl_vector_float_isneg)

(make-vector-isneg gsl-vector-int gsl_vector_int_isneg)

(make-vector-isneg gsl-vector-uint gsl_vector_uint_isneg)

(defgeneric vector-isnonneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are
non-negative, and nil otherwise."))

(defmacro make-vector-isnonneg (type func)
  `(defmethod vector-isnonneg ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-isnonneg gsl-vector-double gsl_vector_isnonneg)

(make-vector-isnonneg gsl-vector-float gsl_vector_float_isnonneg)

(make-vector-isnonneg gsl-vector-int gsl_vector_int_isnonneg)

(make-vector-isnonneg gsl-vector-uint gsl_vector_uint_isnonneg)

(defgeneric vector-equal (u v)
  (:documentation
   "This function returns 1 if the vector u and v are equal and 0
otherwise."))

(defmacro make-vector-equal (type func)
  `(defmethod vector-equal ((u ,type) (v ,type))
     (= (,func (scl::data u) (scl::data v)) 1)))

(make-vector-equal gsl-vector-double gsl_vector_equal)

(make-vector-equal gsl-vector-float gsl_vector_float_equal)

(make-vector-equal gsl-vector-int gsl_vector_int_equal)

(make-vector-equal gsl-vector-uint gsl_vector_uint_equal)

(defgeneric vector-set-sequence (v seq &optional n)
  (:documentation
   "This function sets each element of the vector v to each element of
the sequence seq respectively."))

(defmacro make-vector-set-sequence (type sfunc)
  `(defmethod vector-set-sequence ((v ,type) seq &optional (n nil))
     (dotimes (i (if (null n) (scl::size v) n) v)
       (,sfunc (scl::data v) i (elt seq i)))))

(make-vector-set-sequence gsl-vector-double gsl_vector_set)

(make-vector-set-sequence gsl-vector-float gsl_vector_float_set)

(make-vector-set-sequence gsl-vector-int gsl_vector_int_set)

(make-vector-set-sequence gsl-vector-uint gsl_vector_uint_set)

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

(defgeneric vector-read (v &optional str n)
  (:documentation
   "This function reads into the vector v from the open stream
str. The vector v must be preallocated with the correct length since
the function uses the size of v to determine how many values to
read."))

(defmacro make-vector-read (type sfunc)
  `(defmethod vector-read ((v ,type)
                           &optional (str *standard-input*) (n nil))
     (dotimes (i (if (null n) (scl::size v) n) v)
       (,sfunc (scl::data v) i (read str)))))

(make-vector-read gsl-vector-double gsl_vector_set)

(make-vector-read gsl-vector-float gsl_vector_float_set)

(make-vector-read gsl-vector-int gsl_vector_int_set)

(make-vector-read gsl-vector-uint gsl_vector_uint_set)

(defgeneric vector-write (v &optional str n)
  (:documentation
   "This function writes the elements of the vector v line-by-line to
the stream str."))

(defmacro make-vector-write (type gfunc)
  `(defmethod vector-write ((v ,type)
                            &optional (str *standard-output*) (n nil))
     (let ((s (if (null n) (scl::size v) n)))
       (dotimes (i s v)
         (format str "~S~%" (,gfunc (scl::data v) i))))))

(make-vector-write gsl-vector-double gsl_vector_get)

(make-vector-write gsl-vector-float gsl_vector_float_get)

(make-vector-write gsl-vector-int gsl_vector_int_get)

(make-vector-write gsl-vector-uint gsl_vector_uint_get)

(defvar *print-object-gsl-vector-size* 10)

(defun print-gsl-vector (v stream)
  (format stream "; ~A vector~%" (scl::size v))
  (if (<= (scl::size v) *print-object-gsl-vector-size*)
      (vector-write v stream)
      (progn
        (vector-write v stream *print-object-gsl-vector-size*)
        (format stream "; omitted ~A entries~%"
                (- (scl::size v) *print-object-gsl-vector-size*)))))

;; gsl-vector-any print-object
(defmethod print-object ((v gsl-vector-any) stream)
  (print-gsl-vector v stream)
  (call-next-method))

(defgeneric vector-copy-from-scl-vector (dest src)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest. The two vectors must have the same length."))

(defmacro make-vector-copy-from-scl-vector (gvtype vtype sfunc)
  `(defmethod vector-copy-from-scl-vector ((dest ,gvtype) (src ,vtype))
     (if (not (= (scl::size dest) (scl::size src)))
         (error "vector lengths are not equal")
         (dotimes (i (scl::size src) dest)
           (,sfunc (scl::data dest) i (aref (scl::data src) i))))))

(make-vector-copy-from-scl-vector gsl-vector-double
                                  scl::vector-double
                                  gsl_vector_set)

(make-vector-copy-from-scl-vector gsl-vector-float
                                  scl::vector-float
                                  gsl_vector_float_set)

(make-vector-copy-from-scl-vector gsl-vector-int
                                  scl::vector-int
                                  gsl_vector_int_set)

(make-vector-copy-from-scl-vector gsl-vector-uint
                                  scl::vector-uint
                                  gsl_vector_uint_set)
