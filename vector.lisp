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

(defun vector-alloc (n &key (ctype :double))
  "This function creates a vector of length n, returning a pointer to a newly initialized
vector struct. A new block is allocated for the elements of the vector, and stored in
the block component of the vector struct. The block is owned by the vector, and
will be deallocated when the vector is deallocated."
  (cond ((eq ctype :double)
         (make-instance 'vector-double
                        :entity (gsl_vector_alloc n)
                        :size n
                        :stride 1))
        ((eq ctype :float)
         (make-instance 'vector-float
                        :entity (gsl_vector_float_alloc n)
                        :size n
                        :stride 1))
        (t (error "unknown ctype"))))

(defun vector-calloc (n &key (ctype :double))
  "This function allocates memory for a vector of length n and initializes all the elements
of the vector to zero."
  (cond ((eq ctype :double)
         (make-instance 'vector-double
                        :entity (gsl_vector_calloc n)
                        :size n
                        :stride 1))
        ((eq ctype :float)
         (make-instance 'vector-float
                        :entity (gsl_vector_float_calloc n)
                        :size n
                        :stride 1))
        (t (error "unknown ctype"))))

(defgeneric vector-free (v &optional result)
  (:documentation
   "This function frees a previously allocated vector v. If the vector was created using
gsl-vector-alloc then the block underlying the vector will also be deallocated. If
the vector has been created from another object then the memory is still owned by
that object and will not be deallocated."))

(defmethod vector-free ((v vector-double) &optional (result nil))
  (gsl_vector_free (entity v))
  result)

(defmethod vector-free ((v vector-float) &optional (result nil))
  (gsl_vector_free (entity v))
  result)

(defgeneric vector-get (v i)
  (:documentation
   "This function retruns the i-th element of a vector v. If i lies outside the allowed range
of 0 to n - 1 then the error handler is invoked and 0 is returned."))

(defmethod vector-get ((v vector-double) i)
  (gsl_vector_get (entity v) i))

(defmethod vector-get ((v vector-float) i)
  (gsl_vector_float_get (entity v) i))

(defgeneric vector-set (v i x)
  (:documentation
   "This function sets the value of the i-th element of a vector v to x. If i lies outside
the allowed range of 0 to n - 1 then the error handler is invoked."))

(defmethod vector-set ((v vector-double) i x)
  (gsl_vector_set (entity v) i x)
  v)

(defmethod vector-set ((v vector-float) i x)
  (gsl_vector_float_set (entity v) i x)
  v)

(defgeneric vector-ptr (v i)
  (:documentation
   "This function return a pointer to the i-th element of a vector v. If i lies outside
the allowed range of 0 to n - 1 then the error handler is invoked and a null pointer is
returned."))

(defmethod vector-ptr ((v vector-double) i)
  (gsl_vector_ptr (entity v) i))

(defmethod vector-ptr ((v vector-float) i)
  (gsl_vector_float_ptr (entity v) i))

(defgeneric vector-set-all (v x)
  (:documentation
   "This function sets all the elements of the vector v to the value x."))

(defmethod vector-set-all ((v vector-double) x)
  (gsl_vector_set_all (entity v) x)
  v)

(defmethod vector-set-all ((v vector-float) x)
  (gsl_vector_float_set_all (entity v) x)
  v)

(defgeneric vector-set-zero (v)
  (:documentation
   "This function sets all the elements of the vector v to zero."))

(defmethod vector-set-zero ((v vector-double))
  (gsl_vector_set_zero (entity v))
  v)

(defmethod vector-set-zero ((v vector-float))
  (gsl_vector_float_set_zero (entity v))
  v)

(defgeneric vector-set-basis (v i)
  (:documentation
   "This function makes a basis vector by setting all the elements of the vector v to zero
except for the i-th element which is set to one."))

(defmethod vector-set-basis ((v vector-double) i)
  (gsl_vector_set_basis (entity v) i)
  v)

(defmethod vector-set-basis ((v vector-float) i)
  (gsl_vector_float_set_basis (entity v) i)
  v)

(defgeneric vector-memcpy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the vector dest. The two
vectors must have the same length."))

(defmethod vector-memcpy ((dest vector-double) (src vector-double))
  (gsl_vector_memcpy (entity dest) (entity src))
  dest)

(defmethod vector-memcpy ((dest vector-float) (src vector-float))
  (gsl_vector_float_memcpy (entity dest) (entity src))
  dest)

(defgeneric vector-swap (v w)
  (:documentation
   "This function exhanges the elements of the vectors v and w by copying. The two
vectors must have the same length."))

(defmethod vector-swap ((v vector-double) (w vector-double))
  (gsl_vector_swap (entity v) (entity w))
  (values v w))

(defmethod vector-swap ((v vector-float) (w vector-float))
  (gsl_vector_float_swap (entity v) (entity w))
  (values v w))

(defgeneric vector-swap-elements (v i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the vector v in-place."))

(defmethod vector-swap-elements ((v vector-double) i j)
  (gsl_vector_swap_elements (entity v) i j)
  v)

(defmethod vector-swap-elements ((v vector-float) i j)
  (gsl_vector_float_swap_elements (entity v) i j)
  v)

(defgeneric vector-reverse (v)
  (:documentation
   "This function reverses the order of the elements of the vector v."))

(defmethod vector-reverse ((v vector-double))
  (gsl_vector_reverse (entity v))
  v)

(defmethod vector-reverse ((v vector-float))
  (gsl_vector_float_reverse (entity v))
  v)

(defgeneric vector-add (a b)
  (:documentation
   "This function adds the elements of vector b to the elements of vector a. The result
a_i <- a_i + b_i is stored in a and b remains unchanged. The two vectors must have
the same length."))

(defmethod vector-add ((a vector-double) (b vector-double))
  (gsl_vector_add (entity a) (entity b))
  a)

(defmethod vector-add ((a vector-float) (b vector-float))
  (gsl_vector_float_add (entity a) (entity b))
  a)

(defgeneric vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements of vector a. The
result a_i <- a_i - b_i is stored in a and b remains unchanged. The two vectors must
have the same length."))

(defmethod vector-sub ((a vector-double) (b vector-double))
  (gsl_vector_sub (entity a) (entity b))
  a)

(defmethod vector-sub ((a vector-float) (b vector-float))
  (gsl_vector_float_sub (entity a) (entity b))
  a)

(defgeneric vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements of vector b. The
result a_i <- a_i * b_i is stored in a and b remains unchanged. The two vectors must
have the same length."))

(defmethod vector-mul ((a vector-double) (b vector-double))
  (gsl_vector_mul (entity a) (entity b))
  a)

(defmethod vector-mul ((a vector-float) (b vector-float))
  (gsl_vector_float_mul (entity a) (entity b))
  a)

(defgeneric vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of vector b. The result
a_i <- a_i / b_i is stored in a and b remains unchanged. The two vectors must have the
same length."))

(defmethod vector-div ((a vector-double) (b vector-double))
  (gsl_vector_div (entity a) (entity b))
  a)

(defmethod vector-div ((a vector-float) (b vector-float))
  (gsl_vector_float_div (entity a) (entity b))
  a)

(defgeneric vector-scale (a x)
  (:documentation
   "This function multiplies the elements of vector a by the constant factor x. The result
a_i <- x * a_i is stored in a."))

(defmethod vector-scale ((a vector-double) x)
  (gsl_vector_scale (entity a) x)
  a)

(defmethod vector-scale ((a vector-float) x)
  (gsl_vector_float_scale (entity a) x)
  a)

(defgeneric vector-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the vector a. The result
a_i <- a_i + x is stored in a."))

(defmethod vector-add-constant ((a vector-double) x)
  (gsl_vector_add_constant (entity a) x)
  a)

(defmethod vector-add-constant ((a vector-float) x)
  (gsl_vector_float_add_constant (entity a) x)
  a)

(defgeneric vector-max (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmethod vector-max ((v vector-double))
  (gsl_vector_max (entity v)))

(defmethod vector-max ((v vector-float))
  (gsl_vector_float_max (entity v)))

(defgeneric vector-min (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmethod vector-min ((v vector-double))
  (gsl_vector_min (entity v)))

(defmethod vector-min ((v vector-float))
  (gsl_vector_float_min (entity v)))

(defgeneric vector-minmax (v)
  (:documentation
   "This function returns the minimum and maximum values in the vector v."))

(defmethod vector-minmax ((v vector-double))
  (cffi:with-foreign-objects ((min-out :double)
                              (max-out :double))
    (gsl_vector_minmax (entity v) min-out max-out)
    (values (cffi:mem-ref min-out :double)
            (cffi:mem-ref max-out :double))))

(defmethod vector-minmax ((v vector-float))
  (cffi:with-foreign-objects ((min-out :float)
                              (max-out :float))
    (gsl_vector_float_minmax (entity v) min-out max-out)
    (values (cffi:mem-ref min-out :float)
            (cffi:mem-ref max-out :float))))

(defgeneric vector-max-index (v)
  (:documentation
   "This function returns the index of the maximum value in the vector v. When there
are several equal maximum elements then the lowest index is returned."))

(defmethod vector-max-index ((v vector-double))
  (gsl_vector_max_index (entity v)))

(defmethod vector-max-index ((v vector-float))
  (gsl_vector_float_max_index (entity v)))

(defgeneric vector-min-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum values in the vector v,
storing them in imin and imax. When there are several equal minimum or maximum
elements then the lowest indices are returned."))

(defmethod vector-min-index ((v vector-double))
  (gsl_vector_min_index (entity v)))

(defmethod vector-min-index ((v vector-float))
  (gsl_vector_float_min_index (entity v)))

(defgeneric vector-minmax-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum values in the vector v."))

(defmethod vector-minmax-index ((v vector-double))
  (cffi:with-foreign-objects ((imin :unsigned-int)
                              (imax :unsigned-int))
    (gsl_vector_minmax_index (entity v) imin imax)
    (values (cffi:mem-ref imin :unsigned-int)
            (cffi:mem-ref imax :unsigned-int))))

(defmethod vector-minmax-index ((v vector-float))
  (cffi:with-foreign-objects ((imin :unsigned-int)
                         (imax :unsigned-int))
    (gsl_vector_float_minmax_index (entity v) imin imax)
    (values (cffi:mem-ref imin :unsigned-int)
            (cffi:mem-ref imax :unsigned-int))))

(defgeneric vector-isnull (v)
  (:documentation
   "This function return t if all the elements of the vector v are zero, and nil otherwise."))

(defmethod vector-isnull ((v vector-double))
  (= (gsl_vector_isnull (entity v)) 1))

(defmethod vector-isnull ((v vector-float))
  (= (gsl_vector_float_isnull (entity v)) 1))

(defgeneric vector-ispos (v)
  (:documentation
   "This function return t if all the elements of the vector v are strictly positive,
and nil otherwise."))

(defmethod vector-ispos ((v vector-double))
  (= (gsl_vector_ispos (entity v)) 1))

(defmethod vector-ispos ((v vector-float))
  (= (gsl_vector_float_ispos (entity v)) 1))

(defgeneric vector-isneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are strictly negative,
and nil otherwise."))

(defmethod vector-isneg ((v vector-double))
  (= (gsl_vector_isneg (entity v)) 1))

(defmethod vector-isneg ((v vector-float))
  (= (gsl_vector_float_isneg (entity v)) 1))

(defgeneric vector-isnonneg (v)
  (:documentation
   "This function return t if all the elements of the vector v are non-negative, and
nil otherwise."))

(defmethod vector-isnonneg ((v vector-double))
  (= (gsl_vector_isnonneg (entity v)) 1))

(defmethod vector-isnonneg ((v vector-float))
  (= (gsl_vector_float_isnonneg (entity v)) 1))

(defgeneric vector-equal (u v)
  (:documentation
   "This function returns 1 if the vector u and v are equal and 0 otherwise."))

(defmethod vector-equal ((u vector-double) (v vector-double))
  (= (gsl_vector_equal (entity u) (entity v)) 1))

(defmethod vector-equal ((u vector-float) (v vector-float))
  (= (gsl_vector_float_equal (entity u) (entity v)) 1))

(defgeneric vector-set-sequence (v n seq)
  (:documentation
   "This function sets each element of the vector v to each element of the sequence
seq respectively."))

(defmethod vector-set-sequence ((v vector-double) n seq)
  (dotimes (i n v)
    (gsl_vector_set (entity v) i (elt seq i))))

(defmethod vector-set-sequence ((v vector-float) n seq)
  (dotimes (i n v)
    (gsl_vector_float_set (entity v) i (elt seq i))))

(defun make-vector (n &key (initial-element nil) (initial-contents nil) (ctype :double))
  "This function makes a vector of length n, returning a instance to a newly initialized
vector class. A new block is allocated for the elements of the vector, and stored in
the block component of the vector struct. The block is owned by the vector, and will be
deallocated when the vector is deallocated.
The memory is allocated using vector-calloc, so it can be passed to function which
vector-free."
  (let ((v (vector-calloc n :ctype ctype)))
    (cond ((not (null initial-element))
           (vector-set-all v initial-element))
          ((not (null initial-contents))
           (vector-set-sequence v n initial-contents))
          (t v))))

(defgeneric vector-to-array (v &optional n)
  (:documentation
   "This function return the array whose i-th element is equal to i-th element of the vector."))

(defmethod vector-to-array ((v vector-double) &optional (n nil))
  (let* ((s (if (null n) (size v) n))
         (a (make-array s :element-type 'double-float)))
    (dotimes (i s a)
      (setf (aref a i) (gsl_vector_get (entity v) i)))))

(defmethod vector-to-array ((v vector-float) &optional (n nil))
  (let* ((s (if (null n) (size v) n))
         (a (make-array s :element-type 'single-float)))
    (dotimes (i s a)
      (setf (aref a i) (gsl_vector_float_get (entity v) i)))))

(defgeneric vector-read (v &optional str n)
  (:documentation
   "This function reads into the vector v from the open stream str. The vector v must be
preallocated with the correct length since the function uses the size of v to determine
how many values to read."))

(defmethod vector-read ((v vector-double) &optional (str *standard-input*) (n nil))
  (dotimes (i (if (null n) (size v) n) v)
    (gsl_vector_set (entity v) i (read str))))

(defmethod vector-read ((v vector-float) &optional (str *standard-input*) (n nil))
  (dotimes (i (if (null n) (size v) n) v)
    (gsl_vector_float_set (entity v) i (read str))))

(defgeneric vector-write (v &optional str n)
  (:documentation
   "This function writes the elements of the vector v line-by-line to the stream str."))

(defmethod vector-write ((v vector-double) &optional (str *standard-output*) (n nil))
  (let ((s (if (null n) (size v) n)))
    (format str "; ~A ~A VECTOR~%" s 'double-float)
    (dotimes (i s v)
      (format str "~S~%" (gsl_vector_get (entity v) i)))))

(defmethod vector-write ((v vector-float) &optional (str *standard-output*) (n nil))
  (let ((s (if (null n) (size v) n)))
    (format str "; ~A ~A VECTOR~%" s 'single-float)
    (dotimes (i s v)
      (format str "~S~%" (gsl_vector_float_get (entity v) i)))))

(defgeneric vector-map (func v &optional n)
  (:documentation
   "Apply function to successive elements of vector."))

(defmethod vector-map (func (v vector-double) &optional (n nil))
  (dotimes (i (if (null n) (size v) n) v)
    (gsl_vector_set (entity v) i (funcall func (gsl_vector_get (entity v) i)))))

(defmethod vector-map (func (v vector-float) &optional (n nil))
  (dotimes (i (if (null n) (size v) n) v)
    (gsl_vector_float_set (entity v) i (funcall func (gsl_vector_float_get (entity v) i)))))

(defgeneric vector-reduce (func init v &optional n)
  (:documentation
   "Combine the elements of vector using function."))

(defmethod vector-reduce (func init (v vector-double) &optional (n nil))
  (let ((acc init))
    (dotimes (i (if (null n) (size v) n) acc)
      (setf acc (funcall func acc (gsl_vector_get (entity v) i))))))

(defmethod vector-reduce (func init (v vector-float) &optional (n nil))
  (let ((acc init))
    (dotimes (i (if (null n) (size v) n) acc)
      (setf acc (funcall func acc (gsl_vector_float_get (entity v) i))))))
