;;;; cl-gsl/matrix.lisp
;;;;
;;;; Matrices are defined by a gsl-matrix structure which describes a
;;;; generalized slice of a block. Like a vector it represents a set of
;;;; elements in an area of memory, but uses two indices instead of one.

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

(defclass matrix-t ()
  ((entity :accessor entity :initarg :entity)
   (size1 :accessor size1 :initarg :size1)
   (size2 :accessor size2 :initarg :size2)
   (tda :accessor tda :initarg :tda)))

(defclass matrix-double (matrix-t) ())

(defclass matrix-float (matrix-t) ())

(defclass matrix-int (matrix-t) ())

(defclass matrix-uint (matrix-t) ())

(defun matrix-alloc (n1 n2 &key (element-type :double))
  "This function create a matrix of size n1 rows by n2 columns, returning a pointer
to a newly initialized matrix struct. A new block is allocated for the elements of
the matrix, and stored in the block component of the matrix struct. The bclok is
owned by the matrix, and will be deallocated when the matrix is deallocated."
  (cond ((eql element-type :double)
         (make-instance 'matrix-double
                        :entity (gsl_matrix_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        ((eql element-type :float)
         (make-instance 'matrix-float
                        :entity (gsl_matrix_float_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        ((eql element-type :int)
         (make-instance 'matrix-int
                        :entity (gsl_matrix_int_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        ((eql element-type :unsigned-int)
         (make-instance 'matrix-uint
                        :entity (gsl_matrix_uint_alloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        (t (error "unknown element type"))))

(defun matrix-calloc (n1 n2 &key (element-type :double))
  "This function allocates memory for a matrix of size n1 rows by n2 columns and
initializes all the elements of the matrix to zero."
  (cond ((eql element-type :double)
         (make-instance 'matrix-double
                        :entity (gsl_matrix_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        ((eql element-type :float)
         (make-instance 'matrix-float
                        :entity (gsl_matrix_float_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        ((eql element-type :int)
         (make-instance 'matrix-int
                        :entity (gsl_matrix_int_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        ((eql element-type :unsigned-int)
         (make-instance 'matrix-uint
                        :entity (gsl_matrix_uint_calloc n1 n2)
                        :size1 n1
                        :size2 n2
                        :tda n2))
        (t (error "unknown element type"))))

(defgeneric matrix-free (m &optional result)
  (:documentation
   "This function frees a previously allocated matrix m. If the matrix was created using
gsl_matrix_alloc then the block underlying the matrix will also be deallocated."))

(defmacro make-matrix-free (class c-func)
  `(defmethod matrix-free ((m ,class) &optional (result nil))
     (,c-func (entity m))
     result))

(make-matrix-free matrix-double gsl_matrix_free)

(make-matrix-free matrix-float gsl_matrix_float_free)

(make-matrix-free matrix-int gsl_matrix_int_free)

(make-matrix-free matrix-uint gsl_matrix_uint_free)

(defgeneric matrix-get (m i j)
  (:documentation
   "This function returns the (i,j)-th element of a matrix m. If i or j lie outside the
allowed range of 0 to n1-1 and 0 to n2-1 then the error handler is invoked and 0
is returned. An inline version of this function is used when HAVE_INLINE is defined."))

(defmacro make-matrix-get (class c-func)
  `(defmethod matrix-get ((m ,class) i j)
     (,c-func (entity m) i j)))

(make-matrix-get matrix-double gsl_matrix_get)

(make-matrix-get matrix-float gsl_matrix_float_get)

(make-matrix-get matrix-int gsl_matrix_int_get)

(make-matrix-get matrix-uint gsl_matrix_uint_get)

(defgeneric matrix-set (m i j x)
  (:documentation
   "This function sets the value of the (i,j)-th element of a matrix m to x. If i or j lies
outside the allowed range of 0 to n1-1 and 0 to n2-1 then the error handler is
invoked. An inline version of this function is used when HAVE_INLINE is defined."))

(defmacro make-matrix-set (class c-func)
  `(defmethod matrix-set ((m ,class) i j x)
     (,c-func (entity m) i j x)
     m))

(make-matrix-set matrix-double gsl_matrix_set)

(make-matrix-set matrix-float gsl_matrix_float_set)

(make-matrix-set matrix-int gsl_matrix_int_set)

(make-matrix-set matrix-uint gsl_matrix_uint_set)

(defgeneric matrix-ptr (m i j)
  (:documentation
   "This function return a pointer to the (i,j)-th element of a matrix m. If i or j lie
outside the allowed range of 0 to n1-1 and 0 to n2-1 then the error handler is
invoked and a null pointer is returned."))

(defmacro make-matrix-ptr (class c-func)
  `(defmethod matrix-ptr ((m ,class) i j)
     (,c-func (entity m) i j)))

(make-matrix-ptr matrix-double gsl_matrix_ptr)

(make-matrix-ptr matrix-float gsl_matrix_float_ptr)

(make-matrix-ptr matrix-int gsl_matrix_int_ptr)

(make-matrix-ptr matrix-uint gsl_matrix_uint_ptr)

(defgeneric matrix-set-all (m x)
  (:documentation
   "This function sets all the elements of the matrix m to the value x."))

(defmacro make-matrix-set-all (class c-func)
  `(defmethod matrix-set-all ((m ,class) x)
     (,c-func (entity m) x)
     m))

(make-matrix-set-all matrix-double gsl_matrix_set_all)

(make-matrix-set-all matrix-float gsl_matrix_float_set_all)

(make-matrix-set-all matrix-int gsl_matrix_int_set_all)

(make-matrix-set-all matrix-uint gsl_matrix_uint_set_all)

(defgeneric matrix-set-zero (m)
  (:documentation
   "This function sets all the elements of the matrix m to zero."))

(defmacro make-matrix-set-zero (class c-func)
  `(defmethod matrix-set-zero ((m ,class))
     (,c-func (entity m))
     m))

(make-matrix-set-zero matrix-double gsl_matrix_set_zero)

(make-matrix-set-zero matrix-float gsl_matrix_float_set_zero)

(make-matrix-set-zero matrix-int gsl_matrix_int_set_zero)

(make-matrix-set-zero matrix-uint gsl_matrix_uint_set_zero)

(defgeneric matrix-set-identity (m)
  (:documentation
   "This function sets the elements of the matrix m to the corresponding elements of the
identity matrix m(i,j) = delta(i,j), i.e. a unit diagonal with all off-diagonal
elements zero. This applies to both square and rectangular matrices."))

(defmacro make-matrix-set-identity (class c-func)
  `(defmethod matrix-set-identity ((m ,class))
     (,c-func (entity m))
     m))

(make-matrix-set-identity matrix-double gsl_matrix_set_identity)

(make-matrix-set-identity matrix-float gsl_matrix_float_set_identity)

(make-matrix-set-identity matrix-int gsl_matrix_int_set_identity)

(make-matrix-set-identity matrix-uint gsl_matrix_uint_set_identity)

(defgeneric matrix-memcpy (dest src)
  (:documentation
   "This function copies the elements of the matrix src into the matrix dest. The two
matrices must have the same size."))

(defmacro make-matrix-memcpy (class c-func)
  `(defmethod matrix-memcpy ((dest ,class) (src ,class))
     (,c-func (entity dest) (entity src))
     dest))

(make-matrix-memcpy matrix-double gsl_matrix_memcpy)

(make-matrix-memcpy matrix-float gsl_matrix_float_memcpy)

(make-matrix-memcpy matrix-int gsl_matrix_int_memcpy)

(make-matrix-memcpy matrix-uint gsl_matrix_uint_memcpy)

(defgeneric matrix-swap (m1 m2)
  (:documentation
   "This function exchanges the elements of the matrices m1 and m2 by copying. The
two matrices must have the same size."))

(defmacro make-matrix-swap (class c-func)
  `(defmethod matrix-swap ((m1 ,class) (m2 ,class))
     (,c-func (entity m1) (entity m2))
     (values m1 m2)))

(make-matrix-swap matrix-double gsl_matrix_swap)

(make-matrix-swap matrix-float gsl_matrix_float_swap)

(make-matrix-swap matrix-int gsl_matrix_int_swap)

(make-matrix-swap matrix-uint gsl_matrix_uint_swap)

(defgeneric matrix-get-row (v m i)
  (:documentation
   "This function copies the elements of the i-th row of the matrix m into the vector v.
The length of the vector must be the same as the length of the column."))

(defmacro make-matrix-get-row (v-class m-class c-func)
  `(defmethod matrix-get-row ((v ,v-class) (m ,m-class) i)
     (,c-func (entity v) (entity m) i)
     v))

(make-matrix-get-row vector-double matrix-double gsl_matrix_get_row)

(make-matrix-get-row vector-float matrix-float gsl_matrix_float_get_row)

(make-matrix-get-row vector-int matrix-int gsl_matrix_int_get_row)

(make-matrix-get-row vector-uint matrix-uint gsl_matrix_uint_get_row)

(defgeneric matrix-get-col (v m j)
  (:documentation
   "This function copies the elements of the j-th column of the matrix m into the vector
v. The length of the vector must be the same as the length of the row."))

(defmacro make-matrix-get-col (v-class m-class c-func)
  `(defmethod matrix-get-col ((v ,v-class) (m ,m-class) j)
     (,c-func (entity v) (entity m) j)
     v))

(make-matrix-get-col vector-double matrix-double gsl_matrix_get_col)

(make-matrix-get-col vector-float matrix-float gsl_matrix_float_get_col)

(make-matrix-get-col vector-int matrix-int gsl_matrix_int_get_col)

(make-matrix-get-col vector-uint matrix-uint gsl_matrix_uint_get_col)

(defgeneric matrix-set-row (m i v)
  (:documentation
   "This function copies the elements of the vector v into the i-th row of the matrix m.
The length of the vector must be the same as the length of the column."))

(defmacro make-matrix-set-row (m-class v-class c-func)
  `(defmethod matrix-set-row ((m ,m-class) i (v ,v-class))
     (,c-func (entity m) i (entity v))
     m))

(make-matrix-set-row matrix-double vector-double gsl_matrix_set_row)

(make-matrix-set-row matrix-float vector-float gsl_matrix_float_set_row)

(make-matrix-set-row matrix-int vector-int gsl_matrix_int_set_row)

(make-matrix-set-row matrix-uint vector-uint gsl_matrix_uint_set_row)

(defgeneric matrix-set-col (m j v)
  (:documentation
   "This function copies the elements of the vector v into the j-th column of the matrix
m. The length of the vector must be the same as the length of the row."))

(defmacro make-matrix-set-col (m-class v-class c-func)
  `(defmethod matrix-set-col ((m ,m-class) j (v ,v-class))
     (,c-func (entity m) j (entity v))
     m))

(make-matrix-set-col matrix-double vector-double gsl_matrix_set_col)

(make-matrix-set-col matrix-float vector-float gsl_matrix_float_set_col)

(make-matrix-set-col matrix-int vector-int gsl_matrix_int_set_col)

(make-matrix-set-col matrix-uint vector-uint gsl_matrix_uint_set_col)

(defgeneric matrix-swap-rows (m i j)
  (:documentation
   "This function exchanges the i-th and j-th rows of the matrix m in-place."))

(defmacro make-matrix-swap-rows (class c-func)
  `(defmethod matrix-swap-rows ((m ,class) i j)
     (,c-func (entity m) i j)
     m))

(make-matrix-swap-rows matrix-double gsl_matrix_swap_rows)

(make-matrix-swap-rows matrix-float gsl_matrix_float_swap_rows)

(make-matrix-swap-rows matrix-int gsl_matrix_int_swap_rows)

(make-matrix-swap-rows matrix-uint gsl_matrix_uint_swap_rows)

(defgeneric matrix-swap-columns (m i j)
  (:documentation
   "This function exchanges the i-th and j-th columns of the matrix m in-place."))

(defmacro make-matrix-swap-columns (class c-func)
  `(defmethod matrix-swap-columns ((m ,class) i j)
     (,c-func (entity m) i j)
     m))

(make-matrix-swap-columns matrix-double gsl_matrix_swap_columns)

(make-matrix-swap-columns matrix-float gsl_matrix_float_swap_columns)

(make-matrix-swap-columns matrix-int gsl_matrix_int_swap_columns)

(make-matrix-swap-columns matrix-uint gsl_matrix_uint_swap_columns)

(defgeneric matrix-swap-rowcol (m i j)
  (:documentation
   "This function exchange the i-th row and j-th column of the matrix m inplace. The
matrix must be square for this operation to be possible."))

(defmacro make-matrix-swap-rowcol (class c-func)
  `(defmethod matrix-swap-rowcol ((m ,class) i j)
     (,c-func (entity m) i j)
     m))

(make-matrix-swap-rowcol matrix-double gsl_matrix_swap_rowcol)

(make-matrix-swap-rowcol matrix-float gsl_matrix_float_swap_rowcol)

(make-matrix-swap-rowcol matrix-int gsl_matrix_int_swap_rowcol)

(make-matrix-swap-rowcol matrix-uint gsl_matrix_uint_swap_rowcol)

(defgeneric matrix-transpose-memcpy (dest src)
  (:documentation
   "This function makes the matrix dest the transpose of the matrix src by copying the
elements of src into dest. This function works for all matrices provided that the
dimensions of the matrix dest match the transposed dimensions of the matrix src."))

(defmacro make-matrix-transpose-memcpy (class c-func)
  `(defmethod matrix-transpose-memcpy ((dest ,class) (src ,class))
     (,c-func (entity dest) (entity src))
     dest))

(make-matrix-transpose-memcpy matrix-double gsl_matrix_transpose_memcpy)

(make-matrix-transpose-memcpy matrix-float gsl_matrix_float_transpose_memcpy)

(make-matrix-transpose-memcpy matrix-int gsl_matrix_int_transpose_memcpy)

(make-matrix-transpose-memcpy matrix-uint gsl_matrix_uint_transpose_memcpy)

(defgeneric matrix-transpose (m)
  (:documentation
   "This function replaces the matrix m by its transpose by copying the elements of the
matrix in-place. The matrix must be square for this operation to be possible."))

(defmacro make-matrix-transpose (class c-func)
  `(defmethod matrix-transpose ((m ,class))
     (,c-func (entity m))
     m))

(make-matrix-transpose matrix-double gsl_matrix_transpose)

(make-matrix-transpose matrix-float gsl_matrix_float_transpose)

(make-matrix-transpose matrix-int gsl_matrix_int_transpose)

(make-matrix-transpose matrix-uint gsl_matrix_uint_transpose)

(defgeneric matrix-add (a b)
  (:documentation
   "This fuction adds the elements of matrix b to the elements of matrix a. The result
a(i,j) <- a(i,j) + b(i,j) is stored in a and b remains unchanged. The two matrices
must have the same dimensions."))

(defmacro make-matrix-add (class c-func)
  `(defmethod matrix-add ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-matrix-add matrix-double gsl_matrix_add)

(make-matrix-add matrix-float gsl_matrix_float_add)

(make-matrix-add matrix-int gsl_matrix_int_add)

(make-matrix-add matrix-uint gsl_matrix_uint_add)

(defgeneric matrix-sub (a b)
  (:documentation
   "This function subtracts the elements of matrix b from the elements of matrix a. The
result a(i,j) <- a(i,j) - b(i,j) is stored in a and b remains unchanged."))

(defmacro make-matrix-sub (class c-func)
  `(defmethod matrix-sub ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-matrix-sub matrix-double gsl_matrix_sub)

(make-matrix-sub matrix-float gsl_matrix_float_sub)

(make-matrix-sub matrix-int gsl_matrix_int_sub)

(make-matrix-sub matrix-uint gsl_matrix_uint_sub)

(defgeneric matrix-mul-elements (a b)
  (:documentation
   "This function subtracts the elements of matrix b from the elements of matrix a. The
result a(i,j) <- a(i,j) - b(i,j) is stored in a and b remains unchanged. The two
matrices must have the same dimensions."))

(defmacro make-matrix-mul-elements (class c-func)
  `(defmethod matrix-mul-elements ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-matrix-mul-elements matrix-double gsl_matrix_mul_elements)

(make-matrix-mul-elements matrix-float gsl_matrix_float_mul_elements)

(make-matrix-mul-elements matrix-int gsl_matrix_int_mul_elements)

(make-matrix-mul-elements matrix-uint gsl_matrix_uint_mul_elements)

(defgeneric matrix-div-elements (a b)
  (:documentation
   "This function multiplies the elements of matrix a by the elements of matrix b. The
result a(i,j) <- a(i,j) * b(i,j) is stored in a and b remains unchanged. The two
matrices must have the same dimensions."))

(defmacro make-matrix-div-elements (class c-func)
  `(defmethod matrix-div-elements ((a ,class) (b ,class))
     (,c-func (entity a) (entity b))
     a))

(make-matrix-div-elements matrix-double gsl_matrix_div_elements)

(make-matrix-div-elements matrix-float gsl_matrix_float_div_elements)

(make-matrix-div-elements matrix-int gsl_matrix_int_div_elements)

(make-matrix-div-elements matrix-uint gsl_matrix_uint_div_elements)

(defgeneric matrix-scale (a x)
  (:documentation
   "This function multiplies the elements of matrix a by the constant factor x. The result
a(i,j) <- a(i,j) * x is stored in a."))

(defmacro make-matrix-scale (class c-func)
  `(defmethod matrix-scale ((a ,class) x)
     (,c-func (entity a) x)
     a))

(make-matrix-scale matrix-double gsl_matrix_scale)

(make-matrix-scale matrix-float gsl_matrix_float_scale)

(make-matrix-scale matrix-int gsl_matrix_int_scale)

(make-matrix-scale matrix-uint gsl_matrix_uint_scale)

(defgeneric matrix-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the matrix a. The result
a(i,j) <- a(i,j) + x is stored in a."))

(defmacro make-matrix-add-constant (class c-func)
  `(defmethod matrix-add-constant ((a ,class) x)
     (,c-func (entity a) x)
     a))

(make-matrix-add-constant matrix-double gsl_matrix_add_constant)

(make-matrix-add-constant matrix-float gsl_matrix_float_add_constant)

(make-matrix-add-constant matrix-int gsl_matrix_int_add_constant)

(make-matrix-add-constant matrix-uint gsl_matrix_uint_add_constant)

(defgeneric matrix-max (m)
  (:documentation
   "This function returns the maximum value in the matrix m."))

(defmacro make-matrix-max (class c-func)
  `(defmethod matrix-max ((m ,class))
     (,c-func (entity m))))

(make-matrix-max matrix-double gsl_matrix_max)

(make-matrix-max matrix-float gsl_matrix_float_max)

(make-matrix-max matrix-int gsl_matrix_int_max)

(make-matrix-max matrix-uint gsl_matrix_uint_max)

(defgeneric matrix-min (m)
  (:documentation
   "This function returns the minimum value in the matrix m."))

(defmacro make-matrix-min (class c-func)
  `(defmethod matrix-min ((m ,class))
     (,c-func (entity m))))

(make-matrix-min matrix-double gsl_matrix_min)

(make-matrix-min matrix-float gsl_matrix_float_min)

(make-matrix-min matrix-int gsl_matrix_int_min)

(make-matrix-min matrix-uint gsl_matrix_uint_min)

(defgeneric matrix-minmax (m)
  (:documentation
   "This function returns the minimum and maximum values in the matrix m, storing
them in min-out and max-out."))

(defmacro make-matrix-minmax (class element-type c-func)
  `(defmethod matrix-minmax ((m ,class))
     (cffi:with-foreign-objects ((min-out ,element-type)
                                 (max-out ,element-type))
       (,c-func (entity m) min-out max-out)
       (values (cffi:mem-ref min-out ,element-type)
               (cffi:mem-ref max-out ,element-type)))))

(make-matrix-minmax matrix-double :double gsl_matrix_minmax)

(make-matrix-minmax matrix-float :float gsl_matrix_float_minmax)

(make-matrix-minmax matrix-int :int gsl_matrix_int_minmax)

(make-matrix-minmax matrix-uint :unsigned-int gsl_matrix_uint_minmax)

(defgeneric matrix-max-index (m)
  (:documentation
   "This function returns the indices of the maximum value in the matrix m, storing them
in imax and jmax. When there are several equal maximum elements then the first
element found is returned, searching in row-major order."))

(defmacro make-matrix-max-index (class c-func)
  `(defmethod matrix-max-index ((m ,class))
     (cffi:with-foreign-objects ((imax :unsigned-int)
                                 (jmax :unsigned-int))
       (,c-func (entity m) imax jmax)
       (values (cffi:mem-ref imax :unsigned-int)
               (cffi:mem-ref jmax :unsigned-int)))))

(make-matrix-max-index matrix-double gsl_matrix_max_index)

(make-matrix-max-index matrix-float gsl_matrix_float_max_index)

(make-matrix-max-index matrix-int gsl_matrix_int_max_index)

(make-matrix-max-index matrix-uint gsl_matrix_uint_max_index)

(defgeneric matrix-min-index (m)
  (:documentation
   "This function returns the indices of the minimum value in the matrix m, storing them
in imin and jmin. When there are several equal minimum elements then the first
element found is returned, searching in row-major order."))

(defmacro make-matrix-min-index (class c-func)
  `(defmethod matrix-min-index ((m ,class))
     (cffi:with-foreign-objects ((imin :unsigned-int)
                                 (jmin :unsigned-int))
       (,c-func (entity m) imin jmin)
       (values (cffi:mem-ref imin :unsigned-int)
               (cffi:mem-ref jmin :unsigned-int)))))

(make-matrix-min-index matrix-double gsl_matrix_min_index)

(make-matrix-min-index matrix-float gsl_matrix_float_min_index)

(make-matrix-min-index matrix-int gsl_matrix_int_min_index)

(make-matrix-min-index matrix-uint gsl_matrix_uint_min_index)

(defgeneric matrix-minmax-index (m)
  (:documentation
   "This function returns the indices of the minimum and maximum values in the matrix
m, storing them in (imin,jmin) and (imax,jmax). When there are several equal min-
imum or maximum elements then the first elements found are returned, searching in
row-major order."))

(defmacro make-matrix-minmax-index (class c-func)
  `(defmethod matrix-minmax-index ((m ,class))
     (cffi:with-foreign-objects ((imin :unsigned-int)
                                 (jmin :unsigned-int)
                                 (imax :unsigned-int)
                                 (jmax :unsigned-int))
       (,c-func (entity m) imin jmin imax jmax)
       (values (cffi:mem-ref imin :unsigned-int)
               (cffi:mem-ref jmin :unsigned-int)
               (cffi:mem-ref imax :unsigned-int)
               (cffi:mem-ref jmax :unsigned-int)))))

(make-matrix-minmax-index matrix-double gsl_matrix_minmax_index)

(make-matrix-minmax-index matrix-float gsl_matrix_float_minmax_index)

(make-matrix-minmax-index matrix-int gsl_matrix_int_minmax_index)

(make-matrix-minmax-index matrix-uint gsl_matrix_uint_minmax_index)

(defgeneric matrix-isnull (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are zero,
and nil otherwise."))

(defmacro make-matrix-isnull (class c-func)
  `(defmethod matrix-isnull ((m ,class))
     (= (,c-func (entity m)) 1)))

(make-matrix-isnull matrix-double gsl_matrix_isnull)

(make-matrix-isnull matrix-float gsl_matrix_float_isnull)

(make-matrix-isnull matrix-int gsl_matrix_int_isnull)

(make-matrix-isnull matrix-uint gsl_matrix_uint_isnull)

(defgeneric matrix-ispos (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are strictly positive,
and nil otherwise."))

(defmacro make-matrix-ispos (class c-func)
  `(defmethod matrix-ispos ((m ,class))
     (= (,c-func (entity m)) 1)))

(make-matrix-ispos matrix-double gsl_matrix_ispos)

(make-matrix-ispos matrix-float gsl_matrix_float_ispos)

(make-matrix-ispos matrix-int gsl_matrix_int_ispos)

(make-matrix-ispos matrix-uint gsl_matrix_uint_ispos)

(defgeneric matrix-isneg (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are strictly negative,
and nil otherwise."))

(defmacro make-matrix-isneg (class c-func)
  `(defmethod matrix-isneg ((m ,class))
     (= (,c-func (entity m)) 1)))

(make-matrix-isneg matrix-double gsl_matrix_isneg)

(make-matrix-isneg matrix-float gsl_matrix_float_isneg)

(make-matrix-isneg matrix-int gsl_matrix_int_isneg)

(make-matrix-isneg matrix-uint gsl_matrix_uint_isneg)

(defgeneric matrix-isnonneg (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are non-negative,
and nil otherwise."))

(defmacro make-matrix-isnonneg (class c-func)
  `(defmethod matrix-isnonneg ((m ,class))
     (= (,c-func (entity m)) 1)))

(make-matrix-isnonneg matrix-double gsl_matrix_isnonneg)

(make-matrix-isnonneg matrix-float gsl_matrix_float_isnonneg)

(make-matrix-isnonneg matrix-int gsl_matrix_int_isnonneg)

(make-matrix-isnonneg matrix-uint gsl_matrix_uint_isnonneg)

(defgeneric matrix-equal (a b)
  (:documentation
   "This function returns t if the matrices a and b are equal (by comparison of element
values) and nil otherwise."))

(defmacro make-matrix-equal (class c-func)
  `(defmethod matrix-equal ((a ,class) (b ,class))
     (= (,c-func (entity a) (entity b)) 1)))

(make-matrix-equal matrix-double gsl_matrix_equal)

(make-matrix-equal matrix-float gsl_matrix_float_equal)

(make-matrix-equal matrix-int gsl_matrix_int_equal)

(make-matrix-equal matrix-uint gsl_matrix_uint_equal)

(defun last1 (lst)
  (car (last lst)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defgeneric matrix-set-sequence (m seq &optional n1 n2)
  (:documentation
   "This function sets each element of the matrix m to each element of the sequence
seq respectively."))

(defmacro make-matrix-set-sequence (class set-c-func)
  `(defmethod matrix-set-sequence ((m ,class) seq &optional (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (size1 m) n1))
           (s2 (if (null n2) (size2 m) n2))
           (idx 0))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (,set-c-func (entity m) i j (elt seq idx))
           (setf idx (1+ idx)))))))

(make-matrix-set-sequence matrix-double gsl_matrix_set)

(make-matrix-set-sequence matrix-float gsl_matrix_float_set)

(make-matrix-set-sequence matrix-int gsl_matrix_int_set)

(make-matrix-set-sequence matrix-uint gsl_matrix_uint_set)

(defgeneric matrix-set-2darray (m 2darray &optional n1 n2)
  (:documentation
   "This function sets each element of the matrix m to each element of the 2 dimensions
array respectively."))

(defmacro make-matrix-set-2darray (class set-c-func)
  `(defmethod matrix-set-2darray ((m ,class) 2darray &optional (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (size1 m) n1))
           (s2 (if (null n2) (size2 m) n2)))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (,set-c-func (entity m) i j (aref 2darray i j)))))))

(make-matrix-set-2darray matrix-double gsl_matrix_set)

(make-matrix-set-2darray matrix-float gsl_matrix_float_set)

(make-matrix-set-2darray matrix-int gsl_matrix_int_set)

(make-matrix-set-2darray matrix-uint gsl_matrix_uint_set)

;;; Allocate an aline of (struct gsl-matrix) in foeign heap, and return a pointer to it.
(defun make-matrix (n1 n2 &key (initial-element nil) (initial-contents nil) (element-type :double))
  "This function creates a matrix of size n1 rows by n2 columns, returning a pointer
to a newly initialized matrix struct. A new block is allocated for the elements of
the matrix, and stored in the block component of the matrix struct. The block is
owned by the matrix, and will be deallocated when the matrix is deallocated.
The memory is allocated using gsl-matrix-alloc, so it can be passed to foreign
functions which gsl-matrix-free, or released using free-alien."
  (let ((m (matrix-calloc n1 n2 :element-type element-type)))
    (cond ((not (null initial-element))
           (matrix-set-all m initial-element))
          ((not (null initial-contents))
           (cond ((consp initial-contents)
                  (matrix-set-sequence m (flatten initial-contents) n1 n2))
                 ((and (arrayp initial-contents)
                       (= (length (array-dimensions initial-contents)) 1))
                  (matrix-set-sequence m initial-contents n1 n2))
                 ((and (arrayp initial-contents)
                       (= (length (array-dimensions initial-contents)) 2))
                  (matrix-set-2darray m initial-contents n1 n2))))
          (t m))))

(defgeneric matrix-to-array (m &optional n1 n2)
  (:documentation
   "This function return the array whose elements is equal to elements of the matrix."))

(defmacro make-matrix-to-array (class element-type get-c-func)
  `(defmethod matrix-to-array ((m ,class) &optional (n1 nil) (n2 nil))
     (let* ((s1 (if (null n1) (size1 m) n1))
            (s2 (if (null n2) (size2 m) n2))
            (acc (make-array (* s1 s2) :element-type ,element-type)))
       (dotimes (i s1 acc)
         (dotimes (j s2)
           (setf (aref acc (+ (* i s2) j)) (,get-c-func (entity m) i j)))))))

(make-matrix-to-array matrix-double 'double-float gsl_matrix_get)

(make-matrix-to-array matrix-float 'single-float gsl_matrix_float_get)

(make-matrix-to-array matrix-int
                      `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                      gsl_matrix_int_get)

(make-matrix-to-array matrix-uint
                      `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                      gsl_matrix_uint_get)

(defgeneric matrix-to-2darray (m &optional n1 n2)
  (:documentation
   "This function return the 2darray whose elements is equal to elements of the matrix."))

(defmacro make-matrix-to-2darray (class element-type get-c-func)
  `(defmethod matrix-to-2darray ((m ,class) &optional (n1 nil) (n2 nil))
     (let* ((s1 (if (null n1) (size1 m) n1))
            (s2 (if (null n2) (size2 m) n2))
            (acc (make-array (list s1 s2) :element-type ,element-type)))
       (dotimes (i s1 acc)
         (dotimes (j s2)
           (setf (aref acc i j) (,get-c-func (entity m) i j)))))))

(make-matrix-to-2darray matrix-double 'double-float gsl_matrix_get)

(make-matrix-to-2darray matrix-float 'single-float gsl_matrix_float_get)

(make-matrix-to-2darray matrix-int
                        `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                        gsl_matrix_int_get)

(make-matrix-to-2darray matrix-uint
                        `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                        gsl_matrix_uint_get)

(defgeneric matrix-read (m &optional str n1 n2)
  (:documentation
   "This function reads into the matrix m from the open stream stream in binary format.
The matrix m must be preallocated with the correct dimensions since the function
uses the size of m to determine how many bytes to read."))

(defmacro make-matrix-read (class set-c-func)
  `(defmethod matrix-read ((m ,class) &optional (str *standard-input*) (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (size1 m) n1))
           (s2 (if (null n2) (size2 m) n2)))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (,set-c-func (entity m) i j (read str)))))))

(make-matrix-read matrix-double gsl_matrix_set)

(make-matrix-read matrix-float gsl_matrix_float_set)

(make-matrix-read matrix-int gsl_matrix_int_set)

(make-matrix-read matrix-uint gsl_matrix_uint_set)

(defgeneric matrix-write (m &optional str n1 n2)
  (:documentation
   "This function writes the elements of the matrix m to the stream."))

(defmacro make-matrix-write (class element-type get-c-func)
  `(defmethod matrix-write ((m ,class) &optional (str *standard-output*) (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (size1 m) n1))
           (s2 (if (null n2) (size2 m) n2)))
       (format str "; ~A X ~A ~A MATRIX~%" s1 s2 ,element-type)
       (dotimes (i s1 m)
         (dotimes (j s2)
           (if (eql j (- s2 1))
               (format str "~S~%" (,get-c-func (entity m) i j))
               (format str "~S~C" (,get-c-func (entity m) i j) #\tab)))))))

(make-matrix-write matrix-double :double gsl_matrix_get)

(make-matrix-write matrix-float :float gsl_matrix_float_get)

(make-matrix-write matrix-int :int gsl_matrix_int_get)

(make-matrix-write matrix-uint :unsigned-int gsl_matrix_uint_get)
