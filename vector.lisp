;;;; cl-gsl/vector.lisp
;;;;
;;;; Vectors are defined by a gsl-vector structure which describes a slice of
;;;; a block. Different vectors can be created which point to the same block.
;;;; A vector slice is a set of equally-spaced elements of an area of memory.

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

(defclass vector-t ()
  ((entity :accessor entity :initarg :entity)
   (size :accessor size :initarg :size)
   (stride :accessor stride :initarg :stride)))

(defclass vector-double (vector-t) ())

(defclass vector-float (vector-t) ())

(defclass vector-int (vector-t) ())

(defclass vector-uint (vector-t) ())

(defun vector-alloc (n &key (element-type :double))
  "This function creates a vector of length n, returning a pointer to a newly initialized
vector struct. A new block is allocated for the elements of the vector, and stored in
the block component of the vector struct. The block is owned by the vector, and
will be deallocated when the vector is deallocated."
  (cond ((eql element-type :double)
         (make-instance 'vector-double
                        :entity (gsl_vector_alloc n)
                        :size n
                        :stride 1))
        ((eql element-type :float)
         (make-instance 'vector-float
                        :entity (gsl_vector_float_alloc n)
                        :size n
                        :stride 1))
        ((eql element-type :int)
         (make-instance 'vector-int
                        :entity (gsl_vector_int_alloc n)
                        :size n
                        :stride 1))
        ((eql element-type :unsigned-int)
         (make-instance 'vector-uint
                        :entity (gsl_vector_uint_alloc n)
                        :size n
                        :stride 1))
        (t (error "unknown element type"))))

(defun vector-calloc (n &key (element-type :double))
  "This function allocates memory for a vector of length n and initializes all the elements
of the vector to zero."
  (cond ((eql element-type :double)
         (make-instance 'vector-double
                        :entity (gsl_vector_calloc n)
                        :size n
                        :stride 1))
        ((eql element-type :float)
         (make-instance 'vector-float
                        :entity (gsl_vector_float_calloc n)
                        :size n
                        :stride 1))
        ((eql element-type :int)
         (make-instance 'vector-int
                        :entity (gsl_vector_int_calloc n)
                        :size n
                        :stride 1))
        ((eql element-type :unsigned-int)
         (make-instance 'vector-uint
                        :entity (gsl_vector_uint_calloc n)
                        :size n
                        :stride 1))
        (t (error "unknown element type"))))

(defgeneric vector-free (v &optional result)
  (:documentation
   "This function frees a previously allocated vector v. If the vector was created using
gsl-vector-alloc then the block underlying the vector will also be deallocated. If
the vector has been created from another object then the memory is still owned by
that object and will not be deallocated."))

(defmacro make-vector-free (class c-func)
  `(defmethod vector-free ((v ,class) &optional (result nil))
     (,c-func (entity v))
     result))

(make-vector-free vector-double gsl_vector_free)

(make-vector-free vector-float gsl_vector_float_free)

(make-vector-free vector-int gsl_vector_int_free)

(make-vector-free vector-uint gsl_vector_uint_free)

(defgeneric vector-get (v i)
  (:documentation
   "This function retruns the i-th element of a vector v. If i lies outside the allowed range
of 0 to n - 1 then the error handler is invoked and 0 is returned."))

(defmacro make-vector-get (class c-func)
  `(defmethod vector-get ((v ,class) i)
     (,c-func (entity v) i)))

(make-vector-get vector-double gsl_vector_get)

(make-vector-get vector-float gsl_vector_float_get)

(make-vector-get vector-int gsl_vector_int_get)

(make-vector-get vector-uint gsl_vector_uint_get)

(defgeneric vector-set (v i x)
  (:documentation
   "This function sets the value of the i-th element of a vector v to x. If i lies outside
the allowed range of 0 to n - 1 then the error handler is invoked."))

(defmacro make-vector-set (class c-func)
  `(defmethod vector-set ((v ,class) i x)
     (,c-func (entity v) i x)
     v))

(make-vector-set vector-double gsl_vector_set)

(make-vector-set vector-float gsl_vector_float_set)

(make-vector-set vector-int gsl_vector_int_set)

(make-vector-set vector-uint gsl_vector_uint_set)

(defgeneric vector-ptr (v i)
  (:documentation
   "This function return a pointer to the i-th element of a vector v. If i lies outside
the allowed range of 0 to n - 1 then the error handler is invoked and a null pointer is
returned."))

(defmacro make-vector-ptr (class c-func)
  `(defmethod vector-ptr ((v ,class) i)
     (,c-func (entity v) i)))

(make-vector-ptr vector-double gsl_vector_ptr)

(make-vector-ptr vector-float gsl_vector_float_ptr)

(make-vector-ptr vector-int gsl_vector_int_ptr)

(make-vector-ptr vector-uint gsl_vector_uint_ptr)

(defgeneric vector-set-all (v x)
  (:documentation
   "This function sets all the elements of the vector v to the value x."))

(defmacro make-vector-set-all (class c-func)
  `(defmethod vector-set-all ((v ,class) x)
     (,c-func (entity v) x)
     v))

(make-vector-set-all vector-double gsl_vector_set_all)

(make-vector-set-all vector-float gsl_vector_float_set_all)

(make-vector-set-all vector-int gsl_vector_int_set_all)

(make-vector-set-all vector-uint gsl_vector_uint_set_all)

(defgeneric vector-set-zero (v)
  (:documentation
   "This function sets all the elements of the vector v to zero."))

(defmacro make-vector-set-zero (class c-func)
  `(defmethod vector-set-zero ((v ,class))
     (,c-func (entity v))
     v))

(make-vector-set-zero vector-double gsl_vector_set_zero)

(make-vector-set-zero vector-float gsl_vector_float_set_zero)

(make-vector-set-zero vector-int gsl_vector_int_set_zero)

(make-vector-set-zero vector-uint gsl_vector_uint_set_zero)

(defgeneric vector-set-basis (v i)
  (:documentation
   "This function makes a basis vector by setting all the elements of the vector v to zero
except for the i-th element which is set to one."))

(defmacro make-vector-set-basis (class c-func)
  `(defmethod vector-set-basis ((v ,class) i)
     (,c-func (entity v) i)
     v))

(make-vector-set-basis vector-double gsl_vector_set_basis)

(make-vector-set-basis vector-float gsl_vector_float_set_basis)

(make-vector-set-basis vector-int gsl_vector_int_set_basis)

(make-vector-set-basis vector-uint gsl_vector_uint_set_basis)

(defgeneric vector-memcpy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the vector dest. The two
vectors must have the same length."))

(defmacro make-vector-memcpy (class c-func)
  `(defmethod vector-memcpy ((dest ,class) (src ,class))
     (,c-func (entity dest) (entity src))
     dest))

(make-vector-memcpy vector-double gsl_vector_memcpy)

(make-vector-memcpy vector-float gsl_vector_float_memcpy)

(make-vector-memcpy vector-int gsl_vector_int_memcpy)

(make-vector-memcpy vector-uint gsl_vector_uint_memcpy)

(defgeneric vector-swap (v w)
  (:documentation
   "This function exhanges the elements of the vectors v and w by copying. The two
vectors must have the same length."))

(defmacro make-vector-swap (class c-func)
  `(defmethod vector-swap ((v ,class) (w ,class))
     (,c-func (entity v) (entity w))
     (values v w)))

(make-vector-swap vector-double gsl_vector_swap)

(make-vector-swap vector-float gsl_vector_float_swap)

(make-vector-swap vector-int gsl_vector_int_swap)

(make-vector-swap vector-uint gsl_vector_uint_swap)

(defgeneric vector-swap-elements (v i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the vector v in-place."))

(defmacro make-vector-swap-elements (class c-func)
  `(defmethod vector-swap-elements ((v ,class) i j)
     (,c-func (entity v) i j)
     v))

(make-vector-swap-elements vector-double gsl_vector_swap_elements)

(make-vector-swap-elements vector-float gsl_vector_float_swap_elements)

(make-vector-swap-elements vector-int gsl_vector_int_swap_elements)

(make-vector-swap-elements vector-uint gsl_vector_uint_swap_elements)

(defgeneric vector-reverse (v)
  (:documentation
   "This function reverses the order of the elements of the vector v."))

(defmacro make-vector-reverse (class c-func)
  `(defmethod vector-reverse ((v ,class))
     (,c-func (entity v))
     v))

(make-vector-reverse vector-double gsl_vector_reverse)

(make-vector-reverse vector-float gsl_vector_float_reverse)

(make-vector-reverse vector-int gsl_vector_int_reverse)

(make-vector-reverse vector-uint gsl_vector_uint_reverse)

(defgeneric vector-add (a b)
  (:documentation
   "This function adds the elements of vector b to the elements of vector a. The result
a_i <- a_i + b_i is stored in a and b remains unchanged. The two vectors must have
the same length."))

(defmacro make-vector-add (class c-func)
  `(defmethod vector-add ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-vector-add vector-double gsl_vector_add)

(make-vector-add vector-float gsl_vector_float_add)

(make-vector-add vector-int gsl_vector_int_add)

(make-vector-add vector-uint gsl_vector_uint_add)

(defgeneric vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements of vector a. The
result a_i <- a_i - b_i is stored in a and b remains unchanged. The two vectors must
have the same length."))

(defmacro make-vector-sub (class c-func)
  `(defmethod vector-sub ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-vector-sub vector-double gsl_vector_sub)

(make-vector-sub vector-float gsl_vector_float_sub)

(make-vector-sub vector-int gsl_vector_int_sub)

(make-vector-sub vector-uint gsl_vector_uint_sub)

(defgeneric vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements of vector b. The
result a_i <- a_i * b_i is stored in a and b remains unchanged. The two vectors must
have the same length."))

(defmacro make-vector-mul (class c-func)
  `(defmethod vector-mul ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-vector-mul vector-double gsl_vector_mul)

(make-vector-mul vector-float gsl_vector_float_mul)

(make-vector-mul vector-int gsl_vector_int_mul)

(make-vector-mul vector-uint gsl_vector_uint_mul)

(defgeneric vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of vector b. The result
a_i <- a_i / b_i is stored in a and b remains unchanged. The two vectors must have the
same length."))

(defmacro make-vector-div (class c-func)
  `(defmethod vector-div ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-vector-div vector-double gsl_vector_div)

(make-vector-div vector-float gsl_vector_float_div)

(make-vector-div vector-int gsl_vector_int_div)

(make-vector-div vector-uint gsl_vector_uint_div)

(defgeneric vector-scale (a x)
  (:documentation
   "This function multiplies the elements of vector a by the constant factor x. The result
a_i <- x * a_i is stored in a."))

(defmacro make-vector-scale (class c-func)
  `(defmethod vector-scale ((a ,class) x)
     (,c-func (entity a) x)
     a))

(make-vector-scale vector-double gsl_vector_scale)

(make-vector-scale vector-float gsl_vector_float_scale)

(make-vector-scale vector-int gsl_vector_int_scale)

(make-vector-scale vector-uint gsl_vector_uint_scale)

(defgeneric vector-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the vector a. The result
a_i <- a_i + x is stored in a."))

(defmacro make-vector-add-constant (class c-func)
  `(defmethod vector-add-constant ((a ,class) x)
     (,c-func (entity a) x)
     a))

(make-vector-add-constant vector-double gsl_vector_add_constant)

(make-vector-add-constant vector-float gsl_vector_float_add_constant)

(make-vector-add-constant vector-int gsl_vector_int_add_constant)

(make-vector-add-constant vector-uint gsl_vector_uint_add_constant)

(defgeneric vector-max (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmacro make-vector-max (class c-func)
  `(defmethod vector-max ((v ,class))
     (,c-func (entity v))))

(make-vector-max vector-double gsl_vector_max)

(make-vector-max vector-float gsl_vector_float_max)

(make-vector-max vector-int gsl_vector_int_max)

(make-vector-max vector-uint gsl_vector_uint_max)

(defgeneric vector-min (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmacro make-vector-min (class c-func)
  `(defmethod vector-min ((v ,class))
     (,c-func (entity v))))

(make-vector-min vector-double gsl_vector_min)

(make-vector-min vector-float gsl_vector_float_min)

(make-vector-min vector-int gsl_vector_int_min)

(make-vector-min vector-uint gsl_vector_uint_min)

(defgeneric vector-minmax (v)
  (:documentation
   "This function returns the minimum and maximum values in the vector v."))

(defmacro make-vector-minmax (class element-type c-func)
  `(defmethod vector-minmax ((v ,class))
     (cffi:with-foreign-objects ((min-out ,element-type)
                                 (max-out ,element-type))
       (,c-func (entity v) min-out max-out)
       (values (cffi:mem-ref min-out ,element-type)
               (cffi:mem-ref max-out ,element-type)))))

(make-vector-minmax vector-double :double gsl_vector_minmax)

(make-vector-minmax vector-float :float gsl_vector_float_minmax)

(make-vector-minmax vector-int :int gsl_vector_int_minmax)

(make-vector-minmax vector-uint :unsigned-int gsl_vector_uint_minmax)

(defgeneric vector-max-index (v)
  (:documentation
   "This function returns the index of the maximum value in the vector v. When there
are several equal maximum elements then the lowest index is returned."))

(defmacro make-vector-max-index (class c-func)
  `(defmethod vector-max-index ((v ,class))
     (,c-func (entity v))))

(make-vector-max-index vector-double gsl_vector_max_index)

(make-vector-max-index vector-float gsl_vector_float_max_index)

(make-vector-max-index vector-int gsl_vector_int_max_index)

(make-vector-max-index vector-uint gsl_vector_uint_max_index)

(defgeneric vector-min-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum values in the vector v,
storing them in imin and imax. When there are several equal minimum or maximum
elements then the lowest indices are returned."))

(defmacro make-vector-min-index (class c-func)
  `(defmethod vector-min-index ((v ,class))
     (,c-func (entity v))))

(make-vector-min-index vector-double gsl_vector_min_index)

(make-vector-min-index vector-float gsl_vector_float_min_index)

(make-vector-min-index vector-int gsl_vector_int_min_index)

(make-vector-min-index vector-uint gsl_vector_uint_min_index)

(defgeneric vector-minmax-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum values in the vector v."))

(defmacro make-vector-minmax-index (class c-func)
  `(defmethod vector-minmax-index ((v ,class))
     (cffi:with-foreign-objects ((imin :unsigned-int)
                                 (imax :unsigned-int))
       (,c-func (entity v) imin imax)
       (values (cffi:mem-ref imin :unsigned-int)
               (cffi:mem-ref imax :unsigned-int)))))

(make-vector-minmax-index vector-double gsl_vector_minmax_index)

(make-vector-minmax-index vector-float gsl_vector_float_minmax_index)

(make-vector-minmax-index vector-int gsl_vector_int_minmax_index)

(make-vector-minmax-index vector-uint gsl_vector_uint_minmax_index)

(defgeneric vector-isnull (v)
  (:documentation
   "This function return t if all the elements of the vector v are zero, and nil otherwise."))

(defmacro make-vector-isnull (class c-func)
  `(defmethod vector-isnull ((v ,class))
     (= (,c-func (entity v)) 1)))

(make-vector-isnull vector-double gsl_vector_isnull)

(make-vector-isnull vector-float gsl_vector_float_isnull)

(make-vector-isnull vector-int gsl_vector_int_isnull)

(make-vector-isnull vector-uint gsl_vector_uint_isnull)

(defgeneric vector-ispos (v)
  (:documentation
   "This function return t if all the elements of the vector v are strictly positive,
and nil otherwise."))

(defmacro make-vector-ispos (class c-func)
  `(defmethod vector-ispos ((v ,class))
     (= (,c-func (entity v)) 1)))

(make-vector-ispos vector-double gsl_vector_ispos)

(make-vector-ispos vector-float gsl_vector_float_ispos)

(make-vector-ispos vector-int gsl_vector_int_ispos)

(make-vector-ispos vector-uint gsl_vector_uint_ispos)

(defgeneric vector-isneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are strictly negative,
and nil otherwise."))

(defmacro make-vector-isneg (class c-func)
  `(defmethod vector-isneg ((v ,class))
     (= (,c-func (entity v)) 1)))

(make-vector-isneg vector-double gsl_vector_isneg)

(make-vector-isneg vector-float gsl_vector_float_isneg)

(make-vector-isneg vector-int gsl_vector_int_isneg)

(make-vector-isneg vector-uint gsl_vector_uint_isneg)

(defgeneric vector-isnonneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are non-negative, and
nil otherwise."))

(defmacro make-vector-isnonneg (class c-func)
  `(defmethod vector-isnonneg ((v ,class))
     (= (,c-func (entity v)) 1)))

(make-vector-isnonneg vector-double gsl_vector_isnonneg)

(make-vector-isnonneg vector-float gsl_vector_float_isnonneg)

(make-vector-isnonneg vector-int gsl_vector_int_isnonneg)

(make-vector-isnonneg vector-uint gsl_vector_uint_isnonneg)

(defgeneric vector-equal (u v)
  (:documentation
   "This function returns 1 if the vector u and v are equal and 0 otherwise."))

(defmacro make-vector-equal (class c-func)
  `(defmethod vector-equal ((u ,class) (v ,class))
     (= (,c-func (entity u) (entity v)) 1)))

(make-vector-equal vector-double gsl_vector_equal)

(make-vector-equal vector-float gsl_vector_float_equal)

(make-vector-equal vector-int gsl_vector_int_equal)

(make-vector-equal vector-uint gsl_vector_uint_equal)

(defgeneric vector-set-sequence (v seq &optional n)
  (:documentation
   "This function sets each element of the vector v to each element of the sequence
seq respectively."))

(defmacro make-vector-set-sequence (class set-c-func)
  `(defmethod vector-set-sequence ((v ,class) seq &optional (n nil))
     (dotimes (i (if (null n) (size v) n) v)
       (,set-c-func (entity v) i (elt seq i)))))

(make-vector-set-sequence vector-double gsl_vector_set)

(make-vector-set-sequence vector-float gsl_vector_float_set)

(make-vector-set-sequence vector-int gsl_vector_int_set)

(make-vector-set-sequence vector-uint gsl_vector_uint_set)

(defun make-vector (n &key (initial-element nil) (initial-contents nil) (element-type :double))
  "This function makes a vector of length n, returning a instance to a newly initialized
vector class. A new block is allocated for the elements of the vector, and stored in
the block component of the vector struct. The block is owned by the vector, and will be
deallocated when the vector is deallocated.
The memory is allocated using vector-calloc, so it can be passed to function which
vector-free."
  (let ((v (vector-calloc n :element-type element-type)))
    (cond ((not (null initial-element))
           (vector-set-all v initial-element))
          ((not (null initial-contents))
           (vector-set-sequence v initial-contents n))
          (t v))))

(defgeneric vector-to-array (v &optional n)
  (:documentation
   "This function return the array whose i-th element is equal to i-th element of the vector."))

(defmacro make-vector-to-array (class element-type get-c-func)
  `(defmethod vector-to-array ((v ,class) &optional (n nil))
     (let* ((s (if (null n) (size v) n))
            (a (make-array s :element-type ,element-type)))
       (dotimes (i s a)
         (setf (aref a i) (,get-c-func (entity v) i))))))

(make-vector-to-array vector-double 'double-float gsl_vector_get)

(make-vector-to-array vector-float 'single-float gsl_vector_float_get)

(make-vector-to-array vector-int
                      `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                      gsl_vector_int_get)

(make-vector-to-array vector-uint
                      `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                      gsl_vector_uint_get)

(defgeneric vector-read (v &optional str n)
  (:documentation
   "This function reads into the vector v from the open stream str. The vector v must be
preallocated with the correct length since the function uses the size of v to determine
how many values to read."))

(defmacro make-vector-read (class set-c-func)
  `(defmethod vector-read ((v ,class) &optional (str *standard-input*) (n nil))
     (dotimes (i (if (null n) (size v) n) v)
       (,set-c-func (entity v) i (read str)))))

(make-vector-read vector-double gsl_vector_set)

(make-vector-read vector-float gsl_vector_float_set)

(make-vector-read vector-int gsl_vector_int_set)

(make-vector-read vector-uint gsl_vector_uint_set)

(defgeneric vector-write (v &optional str n)
  (:documentation
   "This function writes the elements of the vector v line-by-line to the stream str."))

(defmacro make-vector-write (class element-type get-c-func)
  `(defmethod vector-write ((v ,class) &optional (str *standard-output*) (n nil))
     (let ((s (if (null n) (size v) n)))
       (format str "; ~A ~A VECTOR~%" s ,element-type)
       (dotimes (i s v)
         (format str "~S~%" (,get-c-func (entity v) i))))))

(make-vector-write vector-double :double gsl_vector_get)

(make-vector-write vector-float :float gsl_vector_float_get)

(make-vector-write vector-int :int gsl_vector_int_get)

(make-vector-write vector-uint :unsigned-int gsl_vector_uint_get)

(defgeneric vector-map (func v &optional n)
  (:documentation
   "Apply function to successive elements of vector."))

(defmacro make-vector-map (class set-c-func get-c-func)
  `(defmethod vector-map (func (v ,class) &optional (n nil))
     (dotimes (i (if (null n) (size v) n) v)
       (,set-c-func (entity v) i (funcall func (,get-c-func (entity v) i))))))

(make-vector-map vector-double gsl_vector_set gsl_vector_get)

(make-vector-map vector-float gsl_vector_float_set gsl_vector_float_get)

(make-vector-map vector-int gsl_vector_int_set gsl_vector_int_get)

(make-vector-map vector-uint gsl_vector_uint_set gsl_vector_uint_get)

(defgeneric vector-reduce (func init v &optional n)
  (:documentation
   "Combine the elements of vector using function."))

(defmacro make-vector-reduce (class get-c-func)
  `(defmethod vector-reduce (func init (v ,class) &optional (n nil))
     (let ((acc init))
       (dotimes (i (if (null n) (size v) n) acc)
         (setf acc (funcall func acc (,get-c-func (entity v) i)))))))

(make-vector-reduce vector-double gsl_vector_get)

(make-vector-reduce vector-float gsl_vector_float_get)

(make-vector-reduce vector-int gsl_vector_int_get)

(make-vector-reduce vector-uint gsl_vector_uint_get)
