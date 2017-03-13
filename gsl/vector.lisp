;;;; gsl/vector.lisp
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


;;; function

(defgeneric vector-get (v i)
  (:documentation
   "This function retruns the i-th element of a vector v. If i lies
outside the allowed range of 0 to n - 1 then the error handler is
invoked and 0 is returned."))

(defmacro make-vector-get (type func)
  `(defmethod vector-get ((v ,type) i)
     (,func (scl::data v) i)))

(make-vector-get vector-double gsl_vector_get)

(make-vector-get vector-float gsl_vector_float_get)

(make-vector-get vector-int gsl_vector_int_get)

(make-vector-get vector-uint gsl_vector_uint_get)

(defgeneric vector-set (v i x)
  (:documentation
   "This function sets the value of the i-th element of a vector v to
x. If i lies outside the allowed range of 0 to n - 1 then the error
handler is invoked."))

(defmacro make-vector-set (type func)
  `(defmethod vector-set ((v ,type) i x)
     (,func (scl::data v) i x)
     v))

(make-vector-set vector-double gsl_vector_set)

(make-vector-set vector-float gsl_vector_float_set)

(make-vector-set vector-int gsl_vector_int_set)

(make-vector-set vector-uint gsl_vector_uint_set)

(defgeneric vector-ptr (v i)
  (:documentation
   "This function return a pointer to the i-th element of a vector
v. If i lies outside the allowed range of 0 to n - 1 then the error
handler is invoked and a null pointer is returned."))

(defmacro make-vector-ptr (type func)
  `(defmethod vector-ptr ((v ,type) i)
     (,func (scl::data v) i)))

(make-vector-ptr vector-double gsl_vector_ptr)

(make-vector-ptr vector-float gsl_vector_float_ptr)

(make-vector-ptr vector-int gsl_vector_int_ptr)

(make-vector-ptr vector-uint gsl_vector_uint_ptr)

(defgeneric vector-set-all (v x)
  (:documentation
   "This function sets all the elements of the vector v to the value
x."))

(defmacro make-vector-set-all (type func)
  `(defmethod vector-set-all ((v ,type) x)
     (,func (scl::data v) x)
     v))

(make-vector-set-all vector-double gsl_vector_set_all)

(make-vector-set-all vector-float gsl_vector_float_set_all)

(make-vector-set-all vector-int gsl_vector_int_set_all)

(make-vector-set-all vector-uint gsl_vector_uint_set_all)

(defgeneric vector-set-zero (v)
  (:documentation
   "This function sets all the elements of the vector v to zero."))

(defmacro make-vector-set-zero (type func)
  `(defmethod vector-set-zero ((v ,type))
     (,func (scl::data v))
     v))

(make-vector-set-zero vector-double gsl_vector_set_zero)

(make-vector-set-zero vector-float gsl_vector_float_set_zero)

(make-vector-set-zero vector-int gsl_vector_int_set_zero)

(make-vector-set-zero vector-uint gsl_vector_uint_set_zero)

(defgeneric vector-set-basis (v i)
  (:documentation
   "This function makes a basis vector by setting all the elements of
the vector v to zero except for the i-th element which is set to
one."))

(defmacro make-vector-set-basis (type func)
  `(defmethod vector-set-basis ((v ,type) i)
     (,func (scl::data v) i)
     v))

(make-vector-set-basis vector-double gsl_vector_set_basis)

(make-vector-set-basis vector-float gsl_vector_float_set_basis)

(make-vector-set-basis vector-int gsl_vector_int_set_basis)

(make-vector-set-basis vector-uint gsl_vector_uint_set_basis)

(defgeneric vector-memcpy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest. The two vectors must have the same length."))

(defmacro make-vector-memcpy (type func)
  `(defmethod vector-memcpy ((dest ,type) (src ,type))
     (,func (scl::data dest) (scl::data src))
     dest))

(make-vector-memcpy vector-double gsl_vector_memcpy)

(make-vector-memcpy vector-float gsl_vector_float_memcpy)

(make-vector-memcpy vector-int gsl_vector_int_memcpy)

(make-vector-memcpy vector-uint gsl_vector_uint_memcpy)

(defgeneric vector-swap (v w)
  (:documentation
   "This function exhanges the elements of the vectors v and w by
copying. The two vectors must have the same length."))

(defmacro make-vector-swap (type func)
  `(defmethod vector-swap ((v ,type) (w ,type))
     (,func (scl::data v) (scl::data w))
     (values v w)))

(make-vector-swap vector-double gsl_vector_swap)

(make-vector-swap vector-float gsl_vector_float_swap)

(make-vector-swap vector-int gsl_vector_int_swap)

(make-vector-swap vector-uint gsl_vector_uint_swap)

(defgeneric vector-swap-elements (v i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the vector v
in-place."))

(defmacro make-vector-swap-elements (type func)
  `(defmethod vector-swap-elements ((v ,type) i j)
     (,func (scl::data v) i j)
     v))

(make-vector-swap-elements vector-double gsl_vector_swap_elements)

(make-vector-swap-elements vector-float gsl_vector_float_swap_elements)

(make-vector-swap-elements vector-int gsl_vector_int_swap_elements)

(make-vector-swap-elements vector-uint gsl_vector_uint_swap_elements)

(defgeneric vector-reverse (v)
  (:documentation
   "This function reverses the order of the elements of the vector v."))

(defmacro make-vector-reverse (type func)
  `(defmethod vector-reverse ((v ,type))
     (,func (scl::data v))
     v))

(make-vector-reverse vector-double gsl_vector_reverse)

(make-vector-reverse vector-float gsl_vector_float_reverse)

(make-vector-reverse vector-int gsl_vector_int_reverse)

(make-vector-reverse vector-uint gsl_vector_uint_reverse)

(defgeneric vector-add (a b)
  (:documentation
   "This function adds the elements of vector b to the elements of
vector a. The result a_i <- a_i + b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-add (type func)
  `(defmethod vector-add ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-add vector-double gsl_vector_add)

(make-vector-add vector-float gsl_vector_float_add)

(make-vector-add vector-int gsl_vector_int_add)

(make-vector-add vector-uint gsl_vector_uint_add)

(defgeneric vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements
of vector a. The result a_i <- a_i - b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-sub (type func)
  `(defmethod vector-sub ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-sub vector-double gsl_vector_sub)

(make-vector-sub vector-float gsl_vector_float_sub)

(make-vector-sub vector-int gsl_vector_int_sub)

(make-vector-sub vector-uint gsl_vector_uint_sub)

(defgeneric vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements
of vector b. The result a_i <- a_i * b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-mul (type func)
  `(defmethod vector-mul ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-mul vector-double gsl_vector_mul)

(make-vector-mul vector-float gsl_vector_float_mul)

(make-vector-mul vector-int gsl_vector_int_mul)

(make-vector-mul vector-uint gsl_vector_uint_mul)

(defgeneric vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of
vector b. The result a_i <- a_i / b_i is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-vector-div (type func)
  `(defmethod vector-div ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-vector-div vector-double gsl_vector_div)

(make-vector-div vector-float gsl_vector_float_div)

(make-vector-div vector-int gsl_vector_int_div)

(make-vector-div vector-uint gsl_vector_uint_div)

(defgeneric vector-scale (a x)
  (:documentation
   "This function multiplies the elements of vector a by the constant
factor x. The result a_i <- x * a_i is stored in a."))

(defmacro make-vector-scale (type func)
  `(defmethod vector-scale ((a ,type) x)
     (,func (scl::data a) x)
     a))

(make-vector-scale vector-double gsl_vector_scale)

(make-vector-scale vector-float gsl_vector_float_scale)

(make-vector-scale vector-int gsl_vector_int_scale)

(make-vector-scale vector-uint gsl_vector_uint_scale)

(defgeneric vector-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
vector a. The result a_i <- a_i + x is stored in a."))

(defmacro make-vector-add-constant (type func)
  `(defmethod vector-add-constant ((a ,type) x)
     (,func (scl::data a) x)
     a))

(make-vector-add-constant vector-double gsl_vector_add_constant)

(make-vector-add-constant vector-float gsl_vector_float_add_constant)

(make-vector-add-constant vector-int gsl_vector_int_add_constant)

(make-vector-add-constant vector-uint gsl_vector_uint_add_constant)

(defgeneric vector-max (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmacro make-vector-max (type func)
  `(defmethod vector-max ((v ,type))
     (,func (scl::data v))))

(make-vector-max vector-double gsl_vector_max)

(make-vector-max vector-float gsl_vector_float_max)

(make-vector-max vector-int gsl_vector_int_max)

(make-vector-max vector-uint gsl_vector_uint_max)

(defgeneric vector-min (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmacro make-vector-min (type func)
  `(defmethod vector-min ((v ,type))
     (,func (scl::data v))))

(make-vector-min vector-double gsl_vector_min)

(make-vector-min vector-float gsl_vector_float_min)

(make-vector-min vector-int gsl_vector_int_min)

(make-vector-min vector-uint gsl_vector_uint_min)

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

(make-vector-minmax vector-double :double gsl_vector_minmax)

(make-vector-minmax vector-float :float gsl_vector_float_minmax)

(make-vector-minmax vector-int :int gsl_vector_int_minmax)

(make-vector-minmax vector-uint :unsigned-int gsl_vector_uint_minmax)

(defgeneric vector-max-index (v)
  (:documentation
   "This function returns the index of the maximum value in the vector
v. When there are several equal maximum elements then the lowest index
is returned."))

(defmacro make-vector-max-index (type func)
  `(defmethod vector-max-index ((v ,type))
     (,func (scl::data v))))

(make-vector-max-index vector-double gsl_vector_max_index)

(make-vector-max-index vector-float gsl_vector_float_max_index)

(make-vector-max-index vector-int gsl_vector_int_max_index)

(make-vector-max-index vector-uint gsl_vector_uint_max_index)

(defgeneric vector-min-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the vector v, storing them in imin and imax. When there are
several equal minimum or maximum elements then the lowest indices are
returned."))

(defmacro make-vector-min-index (type func)
  `(defmethod vector-min-index ((v ,type))
     (,func (scl::data v))))

(make-vector-min-index vector-double gsl_vector_min_index)

(make-vector-min-index vector-float gsl_vector_float_min_index)

(make-vector-min-index vector-int gsl_vector_int_min_index)

(make-vector-min-index vector-uint gsl_vector_uint_min_index)

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

(make-vector-minmax-index vector-double gsl_vector_minmax_index)

(make-vector-minmax-index vector-float gsl_vector_float_minmax_index)

(make-vector-minmax-index vector-int gsl_vector_int_minmax_index)

(make-vector-minmax-index vector-uint gsl_vector_uint_minmax_index)

(defgeneric vector-isnull (v)
  (:documentation
   "This function return t if all the elements of the vector v are
zero, and nil otherwise."))

(defmacro make-vector-isnull (type func)
  `(defmethod vector-isnull ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-isnull vector-double gsl_vector_isnull)

(make-vector-isnull vector-float gsl_vector_float_isnull)

(make-vector-isnull vector-int gsl_vector_int_isnull)

(make-vector-isnull vector-uint gsl_vector_uint_isnull)

(defgeneric vector-ispos (v)
  (:documentation
   "This function return t if all the elements of the vector v are
strictly positive, and nil otherwise."))

(defmacro make-vector-ispos (type func)
  `(defmethod vector-ispos ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-ispos vector-double gsl_vector_ispos)

(make-vector-ispos vector-float gsl_vector_float_ispos)

(make-vector-ispos vector-int gsl_vector_int_ispos)

(make-vector-ispos vector-uint gsl_vector_uint_ispos)

(defgeneric vector-isneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are
strictly negative, and nil otherwise."))

(defmacro make-vector-isneg (type func)
  `(defmethod vector-isneg ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-isneg vector-double gsl_vector_isneg)

(make-vector-isneg vector-float gsl_vector_float_isneg)

(make-vector-isneg vector-int gsl_vector_int_isneg)

(make-vector-isneg vector-uint gsl_vector_uint_isneg)

(defgeneric vector-isnonneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are
non-negative, and nil otherwise."))

(defmacro make-vector-isnonneg (type func)
  `(defmethod vector-isnonneg ((v ,type))
     (= (,func (scl::data v)) 1)))

(make-vector-isnonneg vector-double gsl_vector_isnonneg)

(make-vector-isnonneg vector-float gsl_vector_float_isnonneg)

(make-vector-isnonneg vector-int gsl_vector_int_isnonneg)

(make-vector-isnonneg vector-uint gsl_vector_uint_isnonneg)

(defgeneric vector-equal (u v)
  (:documentation
   "This function returns 1 if the vector u and v are equal and 0
otherwise."))

(defmacro make-vector-equal (type func)
  `(defmethod vector-equal ((u ,type) (v ,type))
     (= (,func (scl::data u) (scl::data v)) 1)))

(make-vector-equal vector-double gsl_vector_equal)

(make-vector-equal vector-float gsl_vector_float_equal)

(make-vector-equal vector-int gsl_vector_int_equal)

(make-vector-equal vector-uint gsl_vector_uint_equal)

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

(make-vector-read vector-double gsl_vector_set)

(make-vector-read vector-float gsl_vector_float_set)

(make-vector-read vector-int gsl_vector_int_set)

(make-vector-read vector-uint gsl_vector_uint_set)

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

(make-vector-write vector-double gsl_vector_get)

(make-vector-write vector-float gsl_vector_float_get)

(make-vector-write vector-int gsl_vector_int_get)

(make-vector-write vector-uint gsl_vector_uint_get)

(defvar *print-object-vector-size* 10)

(defun print-gsl-vector (v stream)
  (format stream "; ~A vector~%" (scl::size v))
  (if (<= (scl::size v) *print-object-vector-size*)
      (vector-write v stream)
      (progn
        (vector-write v stream *print-object-vector-size*)
        (format stream "; omitted ~A entries~%"
                (- (scl::size v) *print-object-vector-size*)))))

;; vector-any print-object
(defmethod print-object ((v vector-any) stream)
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

(make-vector-copy-from-scl-vector vector-double
                                  scl::vector-double
                                  gsl_vector_set)

(make-vector-copy-from-scl-vector vector-float
                                  scl::vector-float
                                  gsl_vector_float_set)

(make-vector-copy-from-scl-vector vector-int
                                  scl::vector-int
                                  gsl_vector_int_set)

(make-vector-copy-from-scl-vector vector-uint
                                  scl::vector-uint
                                  gsl_vector_uint_set)
