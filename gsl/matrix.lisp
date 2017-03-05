;;;; cl-scl/gsl/matrix.lisp
;;;;
;;;; Matrices are defined by a gsl-matrix structure which describes a
;;;; generalized slice of a block. Like a vector it represents a set
;;;; of elements in an area of memory, but uses two indices instead of
;;;; one.

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


;;; gsl-matrix

;; abstract: scl::matrix-t, gsl-matrix-any
(defclass gsl-matrix-any (scl::matrix-t) ())

(defclass gsl-matrix-double (gsl-matrix-any) ())

(defclass gsl-matrix-float (gsl-matrix-any) ())

(defclass gsl-matrix-int (gsl-matrix-any) ())

(defclass gsl-matrix-uint (gsl-matrix-any) ())


;; functions

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

(defgeneric matrix-get (m i j)
  (:documentation
   "This function returns the (i,j)-th element of a matrix m. If i or
j lie outside the allowed range of 0 to n1-1 and 0 to n2-1 then the
error handler is invoked and 0 is returned. An inline version of this
function is used when HAVE_INLINE is defined."))

(defmacro make-matrix-get (type func)
  `(defmethod matrix-get ((m ,type) i j)
     (,func (scl::data m) i j)))

(make-matrix-get gsl-matrix-double gsl_matrix_get)

(make-matrix-get gsl-matrix-float gsl_matrix_float_get)

(make-matrix-get gsl-matrix-int gsl_matrix_int_get)

(make-matrix-get gsl-matrix-uint gsl_matrix_uint_get)

(defgeneric matrix-set (m i j x)
  (:documentation
   "This function sets the value of the (i,j)-th element of a matrix m
to x. If i or j lies outside the allowed range of 0 to n1-1 and 0 to
n2-1 then the error handler is invoked. An inline version of this
function is used when HAVE_INLINE is defined."))

(defmacro make-matrix-set (type func)
  `(defmethod matrix-set ((m ,type) i j x)
     (,func (scl::data m) i j x)
     m))

(make-matrix-set gsl-matrix-double gsl_matrix_set)

(make-matrix-set gsl-matrix-float gsl_matrix_float_set)

(make-matrix-set gsl-matrix-int gsl_matrix_int_set)

(make-matrix-set gsl-matrix-uint gsl_matrix_uint_set)

(defgeneric matrix-ptr (m i j)
  (:documentation
   "This function return a pointer to the (i,j)-th element of a matrix
m. If i or j lie outside the allowed range of 0 to n1-1 and 0 to n2-1
then the error handler is invoked and a null pointer is returned."))

(defmacro make-matrix-ptr (type func)
  `(defmethod matrix-ptr ((m ,type) i j)
     (,func (scl::data m) i j)))

(make-matrix-ptr gsl-matrix-double gsl_matrix_ptr)

(make-matrix-ptr gsl-matrix-float gsl_matrix_float_ptr)

(make-matrix-ptr gsl-matrix-int gsl_matrix_int_ptr)

(make-matrix-ptr gsl-matrix-uint gsl_matrix_uint_ptr)

(defgeneric matrix-set-all (m x)
  (:documentation
   "This function sets all the elements of the matrix m to the value
x."))

(defmacro make-matrix-set-all (type func)
  `(defmethod matrix-set-all ((m ,type) x)
     (,func (scl::data m) x)
     m))

(make-matrix-set-all gsl-matrix-double gsl_matrix_set_all)

(make-matrix-set-all gsl-matrix-float gsl_matrix_float_set_all)

(make-matrix-set-all gsl-matrix-int gsl_matrix_int_set_all)

(make-matrix-set-all gsl-matrix-uint gsl_matrix_uint_set_all)

(defgeneric matrix-set-zero (m)
  (:documentation
   "This function sets all the elements of the matrix m to zero."))

(defmacro make-matrix-set-zero (type func)
  `(defmethod matrix-set-zero ((m ,type))
     (,func (scl::data m))
     m))

(make-matrix-set-zero gsl-matrix-double gsl_matrix_set_zero)

(make-matrix-set-zero gsl-matrix-float gsl_matrix_float_set_zero)

(make-matrix-set-zero gsl-matrix-int gsl_matrix_int_set_zero)

(make-matrix-set-zero gsl-matrix-uint gsl_matrix_uint_set_zero)

(defgeneric matrix-set-identity (m)
  (:documentation
   "This function sets the elements of the matrix m to the
corresponding elements of the identity matrix m(i,j) = delta(i,j),
i.e. a unit diagonal with all off-diagonal elements zero. This applies
to both square and rectangular matrices."))

(defmacro make-matrix-set-identity (type func)
  `(defmethod matrix-set-identity ((m ,type))
     (,func (scl::data m))
     m))

(make-matrix-set-identity gsl-matrix-double gsl_matrix_set_identity)

(make-matrix-set-identity gsl-matrix-float gsl_matrix_float_set_identity)

(make-matrix-set-identity gsl-matrix-int gsl_matrix_int_set_identity)

(make-matrix-set-identity gsl-matrix-uint gsl_matrix_uint_set_identity)

(defgeneric matrix-memcpy (dest src)
  (:documentation
   "This function copies the elements of the matrix src into the
matrix dest. The two matrices must have the same size."))

(defmacro make-matrix-memcpy (type func)
  `(defmethod matrix-memcpy ((dest ,type) (src ,type))
     (,func (scl::data dest) (scl::data src))
     dest))

(make-matrix-memcpy gsl-matrix-double gsl_matrix_memcpy)

(make-matrix-memcpy gsl-matrix-float gsl_matrix_float_memcpy)

(make-matrix-memcpy gsl-matrix-int gsl_matrix_int_memcpy)

(make-matrix-memcpy gsl-matrix-uint gsl_matrix_uint_memcpy)

(defgeneric matrix-swap (m1 m2)
  (:documentation
   "This function exchanges the elements of the matrices m1 and m2 by
copying. The two matrices must have the same size."))

(defmacro make-matrix-swap (type func)
  `(defmethod matrix-swap ((m1 ,type) (m2 ,type))
     (,func (scl::data m1) (scl::data m2))
     (values m1 m2)))

(make-matrix-swap gsl-matrix-double gsl_matrix_swap)

(make-matrix-swap gsl-matrix-float gsl_matrix_float_swap)

(make-matrix-swap gsl-matrix-int gsl_matrix_int_swap)

(make-matrix-swap gsl-matrix-uint gsl_matrix_uint_swap)

(defgeneric matrix-get-row (v m i)
  (:documentation
   "This function copies the elements of the i-th row of the matrix m
into the vector v. The length of the vector must be the same as the
length of the column."))

(defmacro make-matrix-get-row (vtype mtype func)
  `(defmethod matrix-get-row ((v ,vtype) (m ,mtype) i)
     (,func (scl::data v) (scl::data m) i)
     v))

(make-matrix-get-row gsl-vector-double gsl-matrix-double gsl_matrix_get_row)

(make-matrix-get-row gsl-vector-float gsl-matrix-float gsl_matrix_float_get_row)

(make-matrix-get-row gsl-vector-int gsl-matrix-int gsl_matrix_int_get_row)

(make-matrix-get-row gsl-vector-uint gsl-matrix-uint gsl_matrix_uint_get_row)

(defgeneric matrix-get-col (v m j)
  (:documentation
   "This function copies the elements of the j-th column of the matrix
m into the vector v. The length of the vector must be the same as
the length of the row."))

(defmacro make-matrix-get-col (vtype mtype func)
  `(defmethod matrix-get-col ((v ,vtype) (m ,mtype) j)
     (,func (scl::data v) (scl::data m) j)
     v))

(make-matrix-get-col gsl-vector-double gsl-matrix-double gsl_matrix_get_col)

(make-matrix-get-col gsl-vector-float gsl-matrix-float gsl_matrix_float_get_col)

(make-matrix-get-col gsl-vector-int gsl-matrix-int gsl_matrix_int_get_col)

(make-matrix-get-col gsl-vector-uint gsl-matrix-uint gsl_matrix_uint_get_col)

(defgeneric matrix-set-row (m i v)
  (:documentation
   "This function copies the elements of the vector v into the i-th
row of the matrix m. The length of the vector must be the same as
the length of the column."))

(defmacro make-matrix-set-row (mtype vtype func)
  `(defmethod matrix-set-row ((m ,mtype) i (v ,vtype))
     (,func (scl::data m) i (scl::data v))
     m))

(make-matrix-set-row gsl-matrix-double gsl-vector-double gsl_matrix_set_row)

(make-matrix-set-row gsl-matrix-float gsl-vector-float gsl_matrix_float_set_row)

(make-matrix-set-row gsl-matrix-int gsl-vector-int gsl_matrix_int_set_row)

(make-matrix-set-row gsl-matrix-uint gsl-vector-uint gsl_matrix_uint_set_row)

(defgeneric matrix-set-col (m j v)
  (:documentation
   "This function copies the elements of the vector v into the j-th
column of the matrix m. The length of the vector must be the same
as the length of the row."))

(defmacro make-matrix-set-col (mtype vtype func)
  `(defmethod matrix-set-col ((m ,mtype) j (v ,vtype))
     (,func (scl::data m) j (scl::data v))
     m))

(make-matrix-set-col gsl-matrix-double gsl-vector-double gsl_matrix_set_col)

(make-matrix-set-col gsl-matrix-float gsl-vector-float gsl_matrix_float_set_col)

(make-matrix-set-col gsl-matrix-int gsl-vector-int gsl_matrix_int_set_col)

(make-matrix-set-col gsl-matrix-uint gsl-vector-uint gsl_matrix_uint_set_col)

(defgeneric matrix-swap-rows (m i j)
  (:documentation
   "This function exchanges the i-th and j-th rows of the matrix m
in-place."))

(defmacro make-matrix-swap-rows (type func)
  `(defmethod matrix-swap-rows ((m ,type) i j)
     (,func (scl::data m) i j)
     m))

(make-matrix-swap-rows gsl-matrix-double gsl_matrix_swap_rows)

(make-matrix-swap-rows gsl-matrix-float gsl_matrix_float_swap_rows)

(make-matrix-swap-rows gsl-matrix-int gsl_matrix_int_swap_rows)

(make-matrix-swap-rows gsl-matrix-uint gsl_matrix_uint_swap_rows)

(defgeneric matrix-swap-columns (m i j)
  (:documentation
   "This function exchanges the i-th and j-th columns of the matrix m
in-place."))

(defmacro make-matrix-swap-columns (type func)
  `(defmethod matrix-swap-columns ((m ,type) i j)
     (,func (scl::data m) i j)
     m))

(make-matrix-swap-columns gsl-matrix-double gsl_matrix_swap_columns)

(make-matrix-swap-columns gsl-matrix-float gsl_matrix_float_swap_columns)

(make-matrix-swap-columns gsl-matrix-int gsl_matrix_int_swap_columns)

(make-matrix-swap-columns gsl-matrix-uint gsl_matrix_uint_swap_columns)

(defgeneric matrix-swap-rowcol (m i j)
  (:documentation
   "This function exchange the i-th row and j-th column of the matrix
m inplace. The matrix must be square for this operation to be
possible."))

(defmacro make-matrix-swap-rowcol (type func)
  `(defmethod matrix-swap-rowcol ((m ,type) i j)
     (,func (scl::data m) i j)
     m))

(make-matrix-swap-rowcol gsl-matrix-double gsl_matrix_swap_rowcol)

(make-matrix-swap-rowcol gsl-matrix-float gsl_matrix_float_swap_rowcol)

(make-matrix-swap-rowcol gsl-matrix-int gsl_matrix_int_swap_rowcol)

(make-matrix-swap-rowcol gsl-matrix-uint gsl_matrix_uint_swap_rowcol)

(defgeneric matrix-transpose-memcpy (dest src)
  (:documentation
   "This function makes the matrix dest the transpose of the matrix
src by copying the elements of src into dest. This function works for
all matrices provided that the dimensions of the matrix dest match the
transposed dimensions of the matrix src."))

(defmacro make-matrix-transpose-memcpy (type func)
  `(defmethod matrix-transpose-memcpy ((dest ,type) (src ,type))
     (,func (scl::data dest) (scl::data src))
     dest))

(make-matrix-transpose-memcpy gsl-matrix-double gsl_matrix_transpose_memcpy)

(make-matrix-transpose-memcpy gsl-matrix-float gsl_matrix_float_transpose_memcpy)

(make-matrix-transpose-memcpy gsl-matrix-int gsl_matrix_int_transpose_memcpy)

(make-matrix-transpose-memcpy gsl-matrix-uint gsl_matrix_uint_transpose_memcpy)

(defgeneric matrix-transpose (m)
  (:documentation
   "This function replaces the matrix m by its transpose by copying
the elements of the matrix in-place. The matrix must be square for
this operation to be possible."))

(defmacro make-matrix-transpose (type func)
  `(defmethod matrix-transpose ((m ,type))
     (,func (scl::data m))
     m))

(make-matrix-transpose gsl-matrix-double gsl_matrix_transpose)

(make-matrix-transpose gsl-matrix-float gsl_matrix_float_transpose)

(make-matrix-transpose gsl-matrix-int gsl_matrix_int_transpose)

(make-matrix-transpose gsl-matrix-uint gsl_matrix_uint_transpose)

(defgeneric matrix-add (a b)
  (:documentation
   "This fuction adds the elements of matrix b to the elements of
matrix a. The result a(i,j) <- a(i,j) + b(i,j) is stored in a and b
remains unchanged. The two matrices must have the same
dimensions."))

(defmacro make-matrix-add (type func)
  `(defmethod matrix-add ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-matrix-add gsl-matrix-double gsl_matrix_add)

(make-matrix-add gsl-matrix-float gsl_matrix_float_add)

(make-matrix-add gsl-matrix-int gsl_matrix_int_add)

(make-matrix-add gsl-matrix-uint gsl_matrix_uint_add)

(defgeneric matrix-sub (a b)
  (:documentation
   "This function subtracts the elements of matrix b from the elements
of matrix a. The result a(i,j) <- a(i,j) - b(i,j) is stored in a and b
remains unchanged."))

(defmacro make-matrix-sub (type func)
  `(defmethod matrix-sub ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-matrix-sub gsl-matrix-double gsl_matrix_sub)

(make-matrix-sub gsl-matrix-float gsl_matrix_float_sub)

(make-matrix-sub gsl-matrix-int gsl_matrix_int_sub)

(make-matrix-sub gsl-matrix-uint gsl_matrix_uint_sub)

(defgeneric matrix-mul-elements (a b)
  (:documentation
   "This function subtracts the elements of matrix b from the elements
of matrix a. The result a(i,j) <- a(i,j) - b(i,j) is stored in a and b
remains unchanged. The two matrices must have the same dimensions."))

(defmacro make-matrix-mul-elements (type func)
  `(defmethod matrix-mul-elements ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-matrix-mul-elements gsl-matrix-double gsl_matrix_mul_elements)

(make-matrix-mul-elements gsl-matrix-float gsl_matrix_float_mul_elements)

(make-matrix-mul-elements gsl-matrix-int gsl_matrix_int_mul_elements)

(make-matrix-mul-elements gsl-matrix-uint gsl_matrix_uint_mul_elements)

(defgeneric matrix-div-elements (a b)
  (:documentation
   "This function multiplies the elements of matrix a by the elements
of matrix b. The result a(i,j) <- a(i,j) * b(i,j) is stored in a and b
remains unchanged. The two matrices must have the same dimensions."))

(defmacro make-matrix-div-elements (type func)
  `(defmethod matrix-div-elements ((a ,type) (b ,type))
     (,func (scl::data a) (scl::data b))
     a))

(make-matrix-div-elements gsl-matrix-double gsl_matrix_div_elements)

(make-matrix-div-elements gsl-matrix-float gsl_matrix_float_div_elements)

(make-matrix-div-elements gsl-matrix-int gsl_matrix_int_div_elements)

(make-matrix-div-elements gsl-matrix-uint gsl_matrix_uint_div_elements)

(defgeneric matrix-scale (a x)
  (:documentation
   "This function multiplies the elements of matrix a by the constant
factor x. The result a(i,j) <- a(i,j) * x is stored in a."))

(defmacro make-matrix-scale (type func)
  `(defmethod matrix-scale ((a ,type) x)
     (,func (scl::data a) x)
     a))

(make-matrix-scale gsl-matrix-double gsl_matrix_scale)

(make-matrix-scale gsl-matrix-float gsl_matrix_float_scale)

(make-matrix-scale gsl-matrix-int gsl_matrix_int_scale)

(make-matrix-scale gsl-matrix-uint gsl_matrix_uint_scale)

(defgeneric matrix-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
matrix a. The result a(i,j) <- a(i,j) + x is stored in a."))

(defmacro make-matrix-add-constant (type func)
  `(defmethod matrix-add-constant ((a ,type) x)
     (,func (scl::data a) x)
     a))

(make-matrix-add-constant gsl-matrix-double gsl_matrix_add_constant)

(make-matrix-add-constant gsl-matrix-float gsl_matrix_float_add_constant)

(make-matrix-add-constant gsl-matrix-int gsl_matrix_int_add_constant)

(make-matrix-add-constant gsl-matrix-uint gsl_matrix_uint_add_constant)

(defgeneric matrix-max (m)
  (:documentation
   "This function returns the maximum value in the matrix m."))

(defmacro make-matrix-max (type func)
  `(defmethod matrix-max ((m ,type))
     (,func (scl::data m))))

(make-matrix-max gsl-matrix-double gsl_matrix_max)

(make-matrix-max gsl-matrix-float gsl_matrix_float_max)

(make-matrix-max gsl-matrix-int gsl_matrix_int_max)

(make-matrix-max gsl-matrix-uint gsl_matrix_uint_max)

(defgeneric matrix-min (m)
  (:documentation
   "This function returns the minimum value in the matrix m."))

(defmacro make-matrix-min (type func)
  `(defmethod matrix-min ((m ,type))
     (,func (scl::data m))))

(make-matrix-min gsl-matrix-double gsl_matrix_min)

(make-matrix-min gsl-matrix-float gsl_matrix_float_min)

(make-matrix-min gsl-matrix-int gsl_matrix_int_min)

(make-matrix-min gsl-matrix-uint gsl_matrix_uint_min)

(defgeneric matrix-minmax (m)
  (:documentation
   "This function returns the minimum and maximum values in the matrix
m, storing them in min-out and max-out."))

(defmacro make-matrix-minmax (type element-type func)
  `(defmethod matrix-minmax ((m ,type))
     (cffi:with-foreign-objects ((min-out ,element-type)
                                 (max-out ,element-type))
       (,func (scl::data m) min-out max-out)
       (values (cffi:mem-ref min-out ,element-type)
               (cffi:mem-ref max-out ,element-type)))))

(make-matrix-minmax gsl-matrix-double :double gsl_matrix_minmax)

(make-matrix-minmax gsl-matrix-float :float gsl_matrix_float_minmax)

(make-matrix-minmax gsl-matrix-int :int gsl_matrix_int_minmax)

(make-matrix-minmax gsl-matrix-uint :unsigned-int gsl_matrix_uint_minmax)

(defgeneric matrix-max-index (m)
  (:documentation
   "This function returns the indices of the maximum value in the
matrix m, storing them in imax and jmax. When there are several
equal maximum elements then the first element found is returned,
searching in row-major order."))

(defmacro make-matrix-max-index (type func)
  `(defmethod matrix-max-index ((m ,type))
     (cffi:with-foreign-objects ((imax :unsigned-int)
                                 (jmax :unsigned-int))
       (,func (scl::data m) imax jmax)
       (values (cffi:mem-ref imax :unsigned-int)
               (cffi:mem-ref jmax :unsigned-int)))))

(make-matrix-max-index gsl-matrix-double gsl_matrix_max_index)

(make-matrix-max-index gsl-matrix-float gsl_matrix_float_max_index)

(make-matrix-max-index gsl-matrix-int gsl_matrix_int_max_index)

(make-matrix-max-index gsl-matrix-uint gsl_matrix_uint_max_index)

(defgeneric matrix-min-index (m)
  (:documentation
   "This function returns the indices of the minimum value in the
matrix m, storing them in imin and jmin. When there are several equal
minimum elements then the first element found is returned, searching
in row-major order."))

(defmacro make-matrix-min-index (type func)
  `(defmethod matrix-min-index ((m ,type))
     (cffi:with-foreign-objects ((imin :unsigned-int)
                                 (jmin :unsigned-int))
       (,func (scl::data m) imin jmin)
       (values (cffi:mem-ref imin :unsigned-int)
               (cffi:mem-ref jmin :unsigned-int)))))

(make-matrix-min-index gsl-matrix-double gsl_matrix_min_index)

(make-matrix-min-index gsl-matrix-float gsl_matrix_float_min_index)

(make-matrix-min-index gsl-matrix-int gsl_matrix_int_min_index)

(make-matrix-min-index gsl-matrix-uint gsl_matrix_uint_min_index)

(defgeneric matrix-minmax-index (m)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the matrix m, storing them in (imin,jmin)
and (imax,jmax). When there are several equal min- imum or maximum
elements then the first elements found are returned, searching in
row-major order."))

(defmacro make-matrix-minmax-index (type func)
  `(defmethod matrix-minmax-index ((m ,type))
     (cffi:with-foreign-objects ((imin :unsigned-int)
                                 (jmin :unsigned-int)
                                 (imax :unsigned-int)
                                 (jmax :unsigned-int))
       (,func (scl::data m) imin jmin imax jmax)
       (values (cffi:mem-ref imin :unsigned-int)
               (cffi:mem-ref jmin :unsigned-int)
               (cffi:mem-ref imax :unsigned-int)
               (cffi:mem-ref jmax :unsigned-int)))))

(make-matrix-minmax-index gsl-matrix-double gsl_matrix_minmax_index)

(make-matrix-minmax-index gsl-matrix-float gsl_matrix_float_minmax_index)

(make-matrix-minmax-index gsl-matrix-int gsl_matrix_int_minmax_index)

(make-matrix-minmax-index gsl-matrix-uint gsl_matrix_uint_minmax_index)

(defgeneric matrix-isnull (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are
zero, and nil otherwise."))

(defmacro make-matrix-isnull (type func)
  `(defmethod matrix-isnull ((m ,type))
     (= (,func (scl::data m)) 1)))

(make-matrix-isnull gsl-matrix-double gsl_matrix_isnull)

(make-matrix-isnull gsl-matrix-float gsl_matrix_float_isnull)

(make-matrix-isnull gsl-matrix-int gsl_matrix_int_isnull)

(make-matrix-isnull gsl-matrix-uint gsl_matrix_uint_isnull)

(defgeneric matrix-ispos (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are
strictly positive, and nil otherwise."))

(defmacro make-matrix-ispos (type func)
  `(defmethod matrix-ispos ((m ,type))
     (= (,func (scl::data m)) 1)))

(make-matrix-ispos gsl-matrix-double gsl_matrix_ispos)

(make-matrix-ispos gsl-matrix-float gsl_matrix_float_ispos)

(make-matrix-ispos gsl-matrix-int gsl_matrix_int_ispos)

(make-matrix-ispos gsl-matrix-uint gsl_matrix_uint_ispos)

(defgeneric matrix-isneg (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are
strictly negative, and nil otherwise."))

(defmacro make-matrix-isneg (type func)
  `(defmethod matrix-isneg ((m ,type))
     (= (,func (scl::data m)) 1)))

(make-matrix-isneg gsl-matrix-double gsl_matrix_isneg)

(make-matrix-isneg gsl-matrix-float gsl_matrix_float_isneg)

(make-matrix-isneg gsl-matrix-int gsl_matrix_int_isneg)

(make-matrix-isneg gsl-matrix-uint gsl_matrix_uint_isneg)

(defgeneric matrix-isnonneg (m)
  (:documentation
   "This functions return t if all the elements of the matrix m are
non-negative, and nil otherwise."))

(defmacro make-matrix-isnonneg (type func)
  `(defmethod matrix-isnonneg ((m ,type))
     (= (,func (scl::data m)) 1)))

(make-matrix-isnonneg gsl-matrix-double gsl_matrix_isnonneg)

(make-matrix-isnonneg gsl-matrix-float gsl_matrix_float_isnonneg)

(make-matrix-isnonneg gsl-matrix-int gsl_matrix_int_isnonneg)

(make-matrix-isnonneg gsl-matrix-uint gsl_matrix_uint_isnonneg)

(defgeneric matrix-equal (a b)
  (:documentation
   "This function returns t if the matrices a and b are equal (by
comparison of element values) and nil otherwise."))

(defmacro make-matrix-equal (type func)
  `(defmethod matrix-equal ((a ,type) (b ,type))
     (= (,func (scl::data a) (scl::data b)) 1)))

(make-matrix-equal gsl-matrix-double gsl_matrix_equal)

(make-matrix-equal gsl-matrix-float gsl_matrix_float_equal)

(make-matrix-equal gsl-matrix-int gsl_matrix_int_equal)

(make-matrix-equal gsl-matrix-uint gsl_matrix_uint_equal)

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

(defgeneric matrix-read (m &optional str n1 n2)
  (:documentation
   "This function reads into the matrix m from the open stream str
in binary format. The matrix m must be preallocated with the
correct dimensions since the function uses the size of m to
determine how many bytes to read."))

(defmacro make-matrix-read (type func)
  `(defmethod matrix-read ((m ,type)
                           &optional (str *standard-input*)
                             (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (scl::size1 m) n1))
           (s2 (if (null n2) (scl::size2 m) n2)))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (,func (scl::data m) i j (read str)))))))

(make-matrix-read gsl-matrix-double gsl_matrix_set)

(make-matrix-read gsl-matrix-float gsl_matrix_float_set)

(make-matrix-read gsl-matrix-int gsl_matrix_int_set)

(make-matrix-read gsl-matrix-uint gsl_matrix_uint_set)

(defgeneric matrix-write (m &optional str n1 n2)
  (:documentation
   "This function writes the elements of the matrix m to the
stream."))

(defmacro make-matrix-write (type func)
  `(defmethod matrix-write ((m ,type)
                            &optional (str *standard-output*)
                              (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (scl::size1 m) n1))
           (s2 (if (null n2) (scl::size2 m) n2)))
       (dotimes (i s1 m)
         (dotimes (j s2)
           (if (eql j (- s2 1))
               (format str "~S~%" (,func (scl::data m) i j))
               (format str "~S~C" (,func (scl::data m) i j) #\tab)))))))

(make-matrix-write gsl-matrix-double gsl_matrix_get)

(make-matrix-write gsl-matrix-float gsl_matrix_float_get)

(make-matrix-write gsl-matrix-int gsl_matrix_int_get)

(make-matrix-write gsl-matrix-uint gsl_matrix_uint_get)

(defvar *print-object-gsl-matrix-size1* 10)

(defvar *print-object-gsl-matrix-size2* 10)

(defun print-gsl-matrix (m stream)
  (format stream "; ~A x ~A matrix~%" (scl::size1 m) (scl::size2 m))
  (cond ((and (<= (scl::size1 m) *print-object-gsl-matrix-size1*)
              (<= (scl::size2 m) *print-object-gsl-matrix-size2*))
         (matrix-write m stream))
        ((and (> (scl::size1 m) *print-object-gsl-matrix-size1*)
              (<= (scl::size2 m) *print-object-gsl-matrix-size2*))
         (matrix-write m stream *print-object-gsl-matrix-size1* (scl::size2 m))
         (format stream "; omitted ~A rows~%"
                 (- (scl::size1 m) *print-object-gsl-matrix-size1*)))
        ((and (<= (scl::size1 m) *print-object-gsl-matrix-size1*)
              (> (scl::size2 m) *print-object-gsl-matrix-size2*))
         (matrix-write m stream (scl::size1 m) *print-object-gsl-matrix-size2*)
         (format stream "; omitted ~A columns~%"
                 (- (scl::size2 m) *print-object-gsl-matrix-size2*)))
        ((and (> (scl::size1 m) *print-object-gsl-matrix-size1*)
              (> (scl::size2 m) *print-object-gsl-matrix-size2*))
         (matrix-write m stream *print-object-gsl-matrix-size1*
                       *print-object-gsl-matrix-size2*)
         (format stream "; omitted ~A rows and ~A columns~%"
                 (- (scl::size1 m) *print-object-gsl-matrix-size1*)
                 (- (scl::size2 m) *print-object-gsl-matrix-size2*)))))

;; gsl-matrix-any print-object
(defmethod print-object ((m gsl-matrix-any) stream)
  (print-gsl-matrix m stream)
  (call-next-method))

(defgeneric matrix-copy-from-scl-matrix (m sm)
  (:documentation
   "This function copies the elements of the simple-matrix sm into the
matrix m. The two matrices must have the same size."))

(defmacro make-matrix-copy-from-scl-matrix (gmtype mtype sfunc)
  `(defmethod matrix-copy-from-scl-matrix ((dest ,gmtype) (src ,mtype))
     (if (or (not (= (scl::size1 dest) (scl::size1 src)))
             (not (= (scl::size2 dest) (scl::size2 src))))
         (error "matrix sizes are different")
         (dotimes (i (scl::size1 src) dest)
           (dotimes (j (scl::size2 src))
             (,sfunc (scl::data dest) i j
                     (aref (scl::data src) (+ (* i (scl::tda src)) j))))))))

(make-matrix-copy-from-scl-matrix gsl-matrix-double
                                  scl::matrix-double
                                  gsl_matrix_set)

(make-matrix-copy-from-scl-matrix gsl-matrix-float
                                  scl::matrix-float
                                  gsl_matrix_float_set)

(make-matrix-copy-from-scl-matrix gsl-matrix-int
                                  scl::matrix-int
                                  gsl_matrix_int_set)

(make-matrix-copy-from-scl-matrix gsl-matrix-uint
                                  scl::matrix-uint
                                  gsl_matrix_uint_set)
