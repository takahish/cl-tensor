;;;; sbcl-gsl/vectors.lisp
;;;;
;;;; Matrices are defined by a gsl-matrix structure which describes a generalized slice of a
;;;; block. Like a vector it represents a set of elements in an area of memory, but uses two
;;;; indices instead of one.

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

(cl:defpackage "SB-GSL-MATRIX"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL"
        "SB-GSL-BLOCK"
        "SB-GSL-VECTOR")
  (:export "GSL-MATRIX"
           "GSL-MATRIX"
           "GSL-MATRIX-ALLOC"
           "GSL-MATRIX-CALLOC"
           "GSL-MATRIX-FREE"
           "GSL-MATRIX-GET"
           "GSL-MATRIX-SET"
           "GSL-MATRIX-PTR"
           "GSL-MATRIX-SET-ALL"
           "GSL-MATRIX-SET-ZERO"
           "GSL-MATRIX-SET-IDENTITY"
           "GSL-MATRIX-MEMCPY"
           "GSL-MATRIX-SWAP"
           "GSL-MATRIX-GET-ROW"
           "GSL-MATRIX-GET-COL"
           "GSL-MATRIX-SET-ROW"
           "GSL-MATRIX-SET-COL"
           "GSL-MATRIX-SWAP-ROWS"
           "GSL-MATRIX-SWAP-COLUMNS"
           "GSL-MATRIX-SWAP-ROWCOL"
           "GSL-MATRIX-TRANSPOSE-MEMCPY"
           "GSL-MATRIX-TRANSPOSE"
           "GSL-MATRIX-ADD"
           "GSL-MATRIX-SUB"
           "GSL-MATRIX-MUL-ELEMENTS"
           "GSL-MATRIX-DIV-ELEMENTS"
           "GSL-MATRIX-SCALE"
           "GSL-MATRIX-ADD-CONSTANT"
           "GSL-MATRIX-MAX"
           "GSL-MATRIX-MIN"
           "GSL-MATRIX-MINMAX"
           "GSL-MATRIX-MAX-INDEX"
           "GSL-MATRIX-MIN-INDEX"
           "GSL-MATRIX-MINMAX-INDEX"
           "GSL-MATRIX-ISNULL"
           "GSL-MATRIX-ISPOS"
           "GSL-MATRIX-ISNEG"
           "GSL-MATRIX-ISNONNEG"
           "GSL-MATRIX-EQUAL"
           "MATRIX-ROW-SIZE"
           "MATRIX-COLUMN-SIZE"
           "MATRIX-ROW-DIMENSION"
           "MATRIX-DATA"
           "MATRIX-BLOCK"
           "MATRIX-OWNER"
           "MATRIX-ALLOC"
           "MATRIX-CALLOC"
           "MATRIX-FREE"
           "MATRIX-GET"
           "MATRIX-SET"
           "MATRIX-PTR"
           "MATRIX-SET-ALL"
           "MATRIX-SET-ZERO"
           "MATRIX-SET-IDENTITY"
           "MATRIX-MEMCPY"
           "MATRIX-SWAP"
           "MATRIX-GET-ROW"
           "MATRIX-GET-COL"
           "MATRIX-SET-ROW"
           "MATRIX-SET-COL"
           "MATRIX-SWAP-ROWS"
           "MATRIX-SWAP-COLUMNS"
           "MATRIX-SWAP-ROWCOL"
           "MATRIX-TRANSPOSE-MEMCPY"
           "MATRIX-TRANSPOSE"
           "MATRIX-ADD"
           "MATRIX-SUB"
           "MATRIX-MUL-ELEMENTS"
           "MATRIX-DIV-ELEMENTS"
           "MATRIX-SCALE"
           "MATRIX-ADD-CONSTANT"
           "MATRIX-MAX"
           "MATRIX-MIN"
           "MATRIX-MINMAX"
           "MATRIX-MAX-INDEX"
           "MATRIX-MIN-INDEX"
           "MATRIX-MINMAX-INDEX"
           "MATRIX-ISNULL"
           "MATRIX-ISPOS"
           "MATRIX-ISNEG"
           "MATRIX-ISNONNEG"
           "MATRIX-EQUAL"
           "MATRIX-SET-SEQUENCE"
           "MATRIX-SET-2DARRAY"
           "MAKE-MATRIX"
           "MATRIX-2DARRAY"))

(cl:in-package "SB-GSL-MATRIX")

;;; (struct gsl-matrix)
;;;   The gsl-matrix structure contains six components, the two dimensions of the matrix,
;;;   a physical dimension, a pointer to the memory where the elements of the matrix are stored,
;;;   data, a pointer to the block owned by the matrix block, if any, and an ownership flag,
;;;   owner. The physical dimension determines the memory layout and can differ from the
;;;   matrix dimension to allow the use of submatrices.
;;;   Matrices are stored in row-major order, meaning that each row of elements forms a
;;;   contiguous block in memory. This is the standard "C-language ordering" of two-dimensional
;;;   arrays. Note that FORTRAN stores arrays in column-major order. The number of rows is
;;;   size1. The range of valid row indices runs from 0 to size1-1. similary size2 is the number
;;;   of columns. The range of valid column indices runs from 0 to size2-1. The physical row
;;;   dimension tda, or trailing dimension, specifies the size of a row of the matrix as laid
;;;   out in memory.
;;;
;;;   For example, in the following matrix size1 is 3, size2 is 4, and tda is 8. The physical
;;;   memory layout of the matrix begins in the top left hand-corner and proceeds from left to
;;;   right along each row in turn.
;;;
;;;     00 01 02 03 XX XX XX XX
;;;     10 11 12 13 XX XX XX XX
;;;     20 21 22 23 XX XX XX XX
;;;
;;;   Each unused memory location is represented by "XX". The pointer data ives the location
;;;   of the first element of the matrix in memory. The pointer block stores the location of the
;;;   memory block in which the elements of the matrix are located (if any). If the matrix owns
;;;   this block then the owner field is set to one and the block will be deallocated when the
;;;   owner field is zero and any underlying block will not be freed.
(define-alien-type nil
    (struct gsl-matrix
            (size1 size-t)
            (size2 size-t)
            (tda size-t)
            (data (* double))
            (block (* (struct gsl-block)))
            (owner int)))

;;; (gsl-matrix-alloc n1 n2)
;;;   This function create a matrix of size n1 rows by n2 columns, returning a pointer
;;;   to a newly initialized matrix struct. A new block is allocated for the elements of
;;;   the matrix, and stored in the block component of the matrix struct. The bclok is
;;;   "owned" by the matrix, and will be deallocated when the matrix is deallocated.
(define-alien-routine gsl-matrix-alloc
    (* (struct gsl-matrix))
  (n1 size-t)
  (n2 size-t))

;;; (gsl-matrix-calloc n1 n2)
;;;   This function allocates memory for a matrix of size n1 rows by n2 columns and
;;;   initializes all the elements of the matrix to zero.
(define-alien-routine gsl-matrix-calloc
    (* (struct gsl-matrix))
  (n1 size-t)
  (n2 size-t))

;;; (gsl-matrix-free m)
;;;   This function frees a previously allocated matrix m. If the matrix was created using
;;;   gsl-matrix-alloc then the block underlying the matrix will also be deallocated.
(define-alien-routine gsl-matrix-free
    void
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-get m i j)
;;;   This function returns the (i,j)-th element of a matrix m. If i or j lie outside the
;;;   allowed range of 0 to n1-1 and 0 to n2-1 then the error handler is invoked and 0
;;;   is returned. An inline version of this function is used when HAVE_INLINE is defined.
(define-alien-routine gsl-matrix-get
    double
  (m (* (struct gsl-matrix)))
  (i size-t)
  (j size-t))

;;; (gsl-matrix-set m i j x)
;;;   This function sets the value of the (i,j)-th element of a matrix m to x. If i or j lies
;;;   outside the allowed range of 0 to n1-1 and 0 to n2-1 then the error handler is
;;;   invoked. An inline version of this function is used when HAVE_INLINE is defined.
(define-alien-routine gsl-matrix-set
    void
  (m (* (struct gsl-matrix)))
  (i size-t)
  (j size-t)
  (x double))

;;; (gsl-matrix-ptr m i j)
;;;   This function return a pointer to the (i,j)-th element of a matrix m. If i or j lie
;;;   outside the allowed range of 0 to n1-1 and 0 to n2-1 then the error handler is
;;;   invoked and a null pointer is returned. Inline versions of these functions are used
;;;   when HAVE_INLINE is defined.
(define-alien-routine gsl-matrix-ptr
    (* double)
  (m (* (struct gsl-matrix)))
  (i size-t)
  (j size-t))

;;; (gsl-matrix-set-all m x)
;;;   This function sets all the elements of the matrix m to the value x.
(define-alien-routine gsl-matrix-set-all
    void
  (m (* (struct gsl-matrix)))
  (x double))

;;; (gsl-matrix-set-zero m)
;;;   This function sets all the elements of the matrix m to zero.
(define-alien-routine gsl-matrix-set-zero
    void
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-set-identity m)
;;;   This function sets the elements of the matrix m to the corresponding elements of the
;;;   identity matrix m(i,j) = delta(i,j), i.e. a unit diagonal with all off-diagonal
;;;   elements zero. This applies to both square and rectangular matrices.
(define-alien-routine gsl-matrix-set-identity
    void
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-memcpy dest src)
;;;   This function copies the elements of the matrix src into the matrix dest. The two
;;;   matrices must have the same size.
(define-alien-routine gsl-matrix-memcpy
    int
  (dest (* (struct gsl-matrix)))
  (src (* (struct gsl-matrix))))

;;; (gsl-matrix-swap m1 m2)
;;;   This function exchanges the elements of the matrices m1 and m2 by copying. The
;;;   two matrices must have the same size.
(define-alien-routine gsl-matrix-swap
    int
  (m1 (* (struct gsl-matrix)))
  (m2 (* (struct gsl-matrix))))

;;; (gsl-matrix-get-row v m i)
;;;   This function copies the elements of the i-th row of the matrix m into the vector v.
;;;   The length of the vector must be the same as the length of the column.
(define-alien-routine gsl-matrix-get-row
    int
  (v (* (struct gsl-vector)))
  (m (* (struct gsl-matrix)))
  (i size-t))

;;; (gsl-matrix-get-col v m j)
;;;   This function copies the elements of the j-th column of the matrix m into the vector
;;;   v. The length of the vector must be the same as the length of the row.
(define-alien-routine gsl-matrix-get-col
    int
  (v (* (struct gsl-vector)))
  (m (* (struct gsl-matrix)))
  (j size-t))

;;; (gsl-matrix-set-row m i v)
;;;   This function copies the elements of the vector v into the i-th row of the matrix m.
;;;   The length of the vector must be the same as the length of the column.
(define-alien-routine gsl-matrix-set-row
    int
  (m (* (struct gsl-matrix)))
  (i size-t)
  (v (* (struct gsl-vector))))

;;; (gsl-matrix-set-col m j v)
;;;   This function copies the elements of the vector v into the j-th column of the matrix
;;;   m. The length of the vector must be the same as the length of the row
(define-alien-routine gsl-matrix-set-col
    int
  (m (* (struct gsl-matrix)))
  (j size-t)
  (v (* (struct gsl-vector))))

;;; (gsl-matrix-swap-rows m i j)
;;;   This function exchanges the i-th and j-th rows of the matrix m in-place.
(define-alien-routine gsl-matrix-swap-rows
    int
  (m (* (struct gsl-matrix)))
  (i size-t)
  (j size-t))

;;; (gsl-matrix-swap-columns m i j)
;;;   This function exchanges the i-th and j-th columns of the matrix m in-place.
(define-alien-routine gsl-matrix-swap-columns
    int
  (m (* (struct gsl-matrix)))
  (i size-t)
  (j size-t))

;;; (gsl-matrix-swap-rowcol m i j)
;;;   This function exchange the i-th row and j-th column of the matrix m inplace. The
;;;   matrix must be square for this operation to be possible.
(define-alien-routine gsl-matrix-swap-rowcol
    int
  (m (* (struct gsl-matrix)))
  (i size-t)
  (j size-t))

;;; (gsl-matrix-transpose-memcpy dest src)
;;;   This function makes the matrix dest the transpose of the matrix src by copying the
;;;   elements of src into dest. This function works for all matrices provided that the
;;;   dimensions of the matrix dest match the transposed dimensions of the matrix src.
(define-alien-routine gsl-matrix-transpose-memcpy
    int
  (dest (* (struct gsl-matrix)))
  (src (* (struct gsl-matrix))))

;;; (gsl-matrix-transpose m)
;;;   This function replaces the matrix m by its transpose by copying the elements of the
;;;   matrix in-place. The matrix must be square for this operation to be possible.
(define-alien-routine gsl-matrix-transpose
    int
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-add a b)
;;;   This fuction adds the elements of matrix b to the elements of matrix a. The result
;;;   a(i,j) <- a(i,j) + b(i,j) is stored in a and b remains unchanged. The two matrices
;;;   must have the same dimensions.
(define-alien-routine gsl-matrix-add
    int
  (a (* (struct gsl-matrix)))
  (b (* (struct gsl-matrix))))

;;; (gsl-matrix-sub a b)
;;;   This function subtracts the elements of matrix b from the elements of matrix a. The
;;;   result a(i,j) <- a(i,j) - b(i,j) is stored in a and b remains unchanged. The two
;;;   matrices must have the same dimensions.
(define-alien-routine gsl-matrix-sub
    int
  (a (* (struct gsl-matrix)))
  (b (* (struct gsl-matrix))))

;;; (gsl-matrix-mul-elements a b)
;;;   This function multiplies the elements of matrix a by the elements of matrix b. The
;;;   result a(i,j) <- a(i,j) * b(i,j) is stored in a and b remains unchanged. The two
;;;   matrices must have the same dimensions.
(define-alien-routine gsl-matrix-mul-elements
    int
  (a (* (struct gsl-matrix)))
  (b (* (struct gsl-matrix))))

;;; (gsl-matrix-div-elements a b)
;;;   This function divides the elements of matrix a by the elements of matrix b. The result
;;;   a(i,j) <- a(i,j) / b(i,j) is stored in a and b remains unchanged. The two matrices must
;;;   have the same dimensions.
(define-alien-routine gsl-matrix-div-elements
    int
  (a (* (struct gsl-matrix)))
  (b (* (struct gsl-matrix))))

;;; (gsl-matrix-scale a x)
;;;   This function multiplies the elements of matrix a by the constant factor x. The result
;;;   a(i,j) <- a(i,j) * x is stored in a.
(define-alien-routine gsl-matrix-scale
    int
  (a (* (struct gsl-matrix)))
  (x double))

;;; (gsl-matrix-add-constant a x)
;;;   This function adds the constant value x to the elements of the matrix a. The result
;;;   a(i,j) <- a(i,j) + x is stored in a.
(define-alien-routine gsl-matrix-add-constant
    int
  (a (* (struct gsl-matri)))
  (x double))

;;; (gsl-matrix-max m)
;;;   This function returns the maximum value in the matrix m.
(define-alien-routine gsl-matrix-max
    double
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-min m)
;;;   This function returns the minimum value in the matrix m.
(define-alien-routine gsl-matrix-min
    double
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-minmax m min-out max-out)
;;;   This function returns the minimum and maximum values in the matrix m, storing
;;;   them in min-out and max-out.
(define-alien-routine gsl-matrix-minmax
    void
  (m (* (struct gsl-matrix)))
  (min-out (* double))
  (max-out (* double)))

;;; (gsl-matrix-max-index m imax jmax)
;;;   This function returns the indices of the maximum value in the matrix m, storing them
;;;   in imax and jmax. When there are several equal maximum elements then the first
;;;   element found is returned, searching in row-major order.
(define-alien-routine gsl-matrix-max-index
    void
  (m (* (struct gsl-matrix)))
  (imax (* size-t))
  (jmax (* size-t)))

;;; (gsl-matrix-min-index m imin jmin)
;;;   This function returns the indices of the minimum value in the matrix m, storing them
;;;   in imin and jmin. When there are several equal minimum elements then the first
;;;   element found is returned, searching in row-major order.
(define-alien-routine gsl-matrix-min-index
    void
  (m (* (struct gsl-matrix)))
  (imin (* size-t))
  (jmin (* size-t)))

;;; (gsl-matrix-minmax-index m imin jmin imax jmax)
;;;   This function returns the indices of the minimum and maximum values in the matrix
;;;   m, storing them in (imin,jmin) and (imax,jmax). When there are several equal min-
;;;   imu or maximum elements then the first elements found are returned, searching in
;;;   row-major order.
(define-alien-routine gsl-matrix-minmax-index
    void
  (m (* (struct gsl-matrix)))
  (imin (* size-t))
  (jmin (* size-t))
  (imax (* size-t))
  (jmax (* size-t)))

;;; (gsl-matrix-isnull m)
;;; (gsl-matrix-ispos m)
;;; (gsl-matrix-isneg m)
;;; (gsl-matrix-isnonneg m)
;;;   These functions return 1 if all the elements of the matrix m are zero, strictly positive,
;;;   strictly negative, or non-negative respectively, and 0 otherwise. To test whether a
;;;   matrix is positive-definite, use the Cholesky decomposition.
(define-alien-routine gsl-matrix-isnull
    int
  (m (* (struct gsl-matrix))))

(define-alien-routine gsl-matrix-ispos
    int
  (m (* (struct gsl-matrix))))

(define-alien-routine gsl-matrix-isneg
    int
  (m (* (struct gsl-matrix))))

(define-alien-routine gsl-matrix-isnonneg
    int
  (m (* (struct gsl-matrix))))

;;; (gsl-matrix-equal a b)
;;;   This function returns 1 if the matrices a and b are equal (by comparison of element
;;;   values) and 0 otherwise.
(define-alien-routine gsl-matrix-equal
    int
  (a (* (struct gsl-matrix)))
  (b (* (struct gsl-matrix))))

(defun matrix-row-size (m)
  "This function returns the row size1 of a matrix m."
  (slot m 'size1))

(defun matrix-column-size (m)
  "This function returns the column size2 of a matrix m."
  (slot m 'size2))

(defun matrix-row-dimension (m)
  "This function returns the physical row dimension tda of a matrix m."
  (slot m 'tda))

(defun matrix-data (m)
  "This function returns the data of a matrix m."
  (slot m 'data))

(defun matrix-block (m)
  "This function returns the block of a matrix m."
  (slot m 'block))

(defun matrix-owner (m)
  "This function returns the owner of a matrix m."
  (slot m 'owner))

(defun matrix-alloc (n1 n2)
  "This function create a matrix of size n1 rows by n2 columns, returning a pointer
to a newly initialized matrix struct."
  (gsl-matrix-alloc n1 n2))

(defun matrix-calloc (n1 n2)
  "This function allocates memory for a matrix of size n1 rows by n2 columns and
initializes all the elements of the matrix to zero."
  (gsl-matrix-calloc n1 n2))

(defun matrix-free (m &optional (rt nil))
  "This function frees a previously allocated matrix m. If the matrix was created using
gsl-matrix-alloc then the block underlying the matrix will also be deallocated."
  (progn (gsl-matrix-free m)
         rt))

(defun matrix-get (m i j)
  "This function returns the (i,j)-th element of a matrix m."
  (if (or (< i 0) (< (- (matrix-row-size m) 1) i)
          (< j 0) (< (- (matrix-column-size m) 1) j))
      (error "i and j must lie inside the range of 0 to (- row-size 1) and 0 to (- column-size 1)")
      (gsl-matrix-get m i j)))

(defun matrix-set (m i j x)
  "This function sets the value of the (i,j)-th element of a matrix m to x."
  (if (or (< i 0) (< (- (matrix-row-size m) 1) i)
          (< j 0) (< (- (matrix-column-size m) 1) j))
      (error "i and j must lie inside the range of 0 to (- row-size 1) and 0 to (- column-size 1)")
      (progn (gsl-matrix-set m i j (coerce x 'double-float))
             m)))

(defun matrix-ptr (m i j)
  "This function return a pointer to the (i,j)-th element of a matrix m."
  (if (or (< i 0) (< (- (matrix-row-size m) 1) i)
          (< j 0) (< (- (matrix-column-size m) 1) j))
      (error "i and j must lie inside the range of 0 to (- row-size 1) and 0 to (- column-size 1)")
      (gsl-matrix-ptr m i j)))

(defun matrix-set-all (m x)
  "This function sets all the elements of the matrix m to the value x."
  (progn (gsl-matrix-set-all m (coerce x 'double-float))
         m))

(defun matrix-set-zero (m)
  "This function sets all the elements of the matrix m to zero."
  (progn (gsl-matrix-set-zero m)
         m))

(defun matrix-set-identity (m)
  "This function sets the elements of the matrix m to the corresponding elements of the
identity matrix m(i,j) = delta(i,j), i.e. a unit diagonal with all off-diagonal
elements zero. This applies to both square and rectangular matrices."
  (progn (gsl-matrix-set-identity m)
         m))

(defun matrix-memcpy (dest src)
  "This function copies the elements of the matrix src into the matrix dest."
  (if (or (not (= (matrix-row-size dest) (matrix-row-size src)))
          (not (= (matrix-column-size dest) (matrix-column-size src))))
      (error "the two matrices must have the same size")
      (progn (gsl-matrix-memcpy dest src)
             dest)))

(defun matrix-swap (m1 m2)
  "This function exchanges the elements of the matrices m1 and m2 by copying."
  (if (or (not (= (matrix-row-size m1) (matrix-row-size m2)))
          (not (= (matrix-column-size m1) (matrix-column-size m2))))
      (error "the two matrices must have the same size")
      (progn (gsl-matrix-swap m1 m2)
             (values m1 m2))))

(defun matrix-get-row (v m i)
  "This function copies the elements of the i-th row of the matrix m."
  (cond ((or (< i 0) (< (- (matrix-row-size m) 1) i))
         (error "i must lie inside the range of 0 to (- row-size 1)"))
        ((not (= (vector-size v) (matrix-column-size m)))
         (error "the length of the vector must be the same as the length of the column"))
        (t
         (progn (gsl-matrix-get-row v m i)
                v))))

(defun matrix-get-col (v m j)
  "This function copies the elements of the j-th column of the matrix m."
  (cond ((or (< j 0) (< (- (matrix-column-size m) 1) j))
         (error "j must lie inside the range of 0 to (- column-size 1)"))
        ((not (= (vector-size v) (matrix-row-size m)))
         (error "the length of the vector must be the same as the length of the row"))
        (t
         (progn (gsl-matrix-get-col v m j)
                v))))

(defun matrix-set-row (m i v)
  "This function copies the elements of the vector v into the i-th row of the matrix m."
  (cond ((or (< i 0) (< (- (matrix-row-size m) 1) i))
         (error "i must lie inside the range of 0 to (- row-size 1)"))
        ((not (= (matrix-column-size m) (vector-size v)))
         (error "the length of the vector must be the same as the length of the column"))
        (t
         (progn (gsl-matrix-set-row m i v)
                m))))

(defun matrix-set-col (m j v)
  "This function copies the elements of the vector v into the j-th column of the matrix m."
  (cond ((or (< j 0) (< (- (matrix-column-size m) 1) j))
         (error "j must lie inside the range of 0 to (- column-size 1)"))
        ((not (= (matrix-row-size m) (vector-size v)))
         (error "the length of the vector must be the same as the length of the row"))
        (t
         (progn (gsl-matrix-set-col m j v)
                m))))

(defun matrix-swap-rows (m i j)
  "This function exchanges the i-th and j-th rows of the matrix m in-place."
  (if (or (= i j)
          (< i 0) (< (- (matrix-row-size m) 1) i)
          (< j 0) (< (- (matrix-row-size m) 1) j))
      (error "i and j must be different and lie inside the range of 0 to (- row-size 1)")
      (progn (gsl-matrix-swap-rows m i j)
             m)))

(defun matrix-swap-columns (m i j)
  "This function exchanges the i-th and j-th columns of the matrix m in-place."
  (if (or (= i j)
          (< i 0) (< (- (matrix-column-size m) 1) i)
          (< j 0) (< (- (matrix-column-size m) 1) j))
      (error "i and j must be different and lie inside the range of 0 to (- column-size 1)")
      (progn (gsl-matrix-swap-columns m i j)
             m)))

(defun matrix-swap-rowcol (m i j)
  "This function exchange the i-th row and j-th column of the matrix m inplace. The
matrix must be square for this operation to be possible."
  (if (or (< i 0) (< (- (matrix-row-size m) 1) i)
          (< j 0) (< (- (matrix-column-size m) 1) j))
      (error "i and j must lie inside the range of 0 to (- row-size 1) and 0 to (- column-size 1)")
      (progn (gsl-matrix-swap-rowcol m i j)
             m)))

(defun matrix-transpose-memcpy (dest src)
  "This function makes the matrix dest the transpose of the matrix src by copying the
elements of src into dest."
  (if (or (not (= (matrix-row-size dest) (matrix-column-size src)))
          (not (= (matrix-column-size dest) (matrix-row-size src))))
      (error "the dimensions of the matrix dest match the transposed dimensions of the matrix src")
      (progn (gsl-matrix-transpose-memcpy dest src)
             dest)))

(defun matrix-transpose (m)
  "This function replaces the matrix m by its transpose by copying the elements of the
matrix in-place. The matrix must be square for this operation to be possible."
  (progn (gsl-matrix-transpose m)
         m))

(defun matrix-add (a b)
  "This fuction adds the elements of matrix b to the elements of matrix a. The result
a(i,j) <- a(i,j) + b(i,j) is stored in a and b remains unchanged."
  (if (or (not (= (matrix-row-size a) (matrix-row-size b)))
          (not (= (matrix-column-size a) (matrix-column-size b))))
      (error "the two matrices must have the same dimensions")
      (progn (gsl-matrix-add a b)
             a)))

(defun matrix-sub (a b)
  "This function subtracts the elements of matrix b from the elements of matrix a. The
result a(i,j) <- a(i,j) - b(i,j) is stored in a and b remains unchanged."
  (if (or (not (= (matrix-row-size a) (matrix-row-size b)))
          (not (= (matrix-column-size a) (matrix-column-size b))))
      (error "the two matrices must have the same dimensions")
      (progn (gsl-matrix-sub a b)
             a)))

(defun matrix-mul-elements (a b)
  "This function multiplies the elements of matrix a by the elements of matrix b. The
result a(i,j) <- a(i,j) * b(i,j) is stored in a and b remains unchanged."
  (if (or (not (= (matrix-row-size a) (matrix-row-size b)))
          (not (= (matrix-column-size a) (matrix-column-size b))))
      (error "the two matrices must have the same dimensions")
      (progn (gsl-matrix-mul-elements a b)
             a)))

(defun matrix-div-elements (a b)
  "This function divides the elements of matrix a by the elements of matrix b. The result
a(i,j) <- a(i,j) / b(i,j) is stored in a and b remains unchanged."
  (if (or (not (= (matrix-row-size a) (matrix-row-size b)))
          (not (= (matrix-column-size a) (matrix-column-size b))))
      (error "the two matrices must have the same dimensions")
      (progn (gsl-matrix-div-elements a b)
             a)))

(defun matrix-scale (a x)
  "This function multiplies the elements of matrix a by the constant factor x. The result
a(i,j) <- a(i,j) * x is stored in a."
  (progn (gsl-matrix-scale a (coerce x 'double-float))
         a))

(defun matrix-add-constant (a x)
  "This function adds the constant value x to the elements of the matrix a. The result
a(i,j) <- a(i,j) + x is stored in a."
  (progn (gsl-matrix-add-constant a (coerce x 'double-float))
         a))

(defun matrix-max (m)
  "This function returns the maximum value in the matrix m."
  (gsl-matrix-max m))

(defun matrix-min (m)
  "This function returns the minimum value in the matrix m."
  (gsl-matrix-min m))

(defun matrix-minmax (m)
  "This function returns the minimum and maximum values in the matrix m."
  (with-alien ((min-out double)
               (max-out double))
    (progn (gsl-matrix-minmax m (addr min-out) (addr max-out))
           (values min-out max-out))))

(defun matrix-max-index (m)
  "This function returns the indices of the maximum value in the matrix m."
  (with-alien ((imax size-t)
               (jmax size-t))
    (progn (gsl-matrix-max-index m (addr imax) (addr jmax))
           (values imax jmax))))

(defun matrix-min-index (m)
  "This function returns the indices of the minimum value in the matrix m."
  (with-alien ((imin size-t)
               (jmin size-t))
    (progn (gsl-matrix-min-index m (addr imin) (addr jmin))
           (values imin jmin))))

(defun matrix-minmax-index (m)
  "This function returns the indices of the minimum and maximum values in the matrix m."
  (with-alien ((imin size-t)
               (jmin size-t)
               (imax size-t)
               (jmax size-t))
    (progn (gsl-matrix-minmax-index m
                                    (addr imin)
                                    (addr jmin)
                                    (addr imax)
                                    (addr jmax))
           (values imin jmin imax jmax))))

(defun matrix-isnull (m)
  "This functions return t if all the elements of the matrix m are zero,
and nil otherwise."
  (= (gsl-matrix-isnull m) 1))

(defun matrix-ispos (m)
  "This functions return t if all the elements of the matrix m are strictly positive,
and nil otherwise."
  (= (gsl-matrix-ispos m) 1))

(defun matrix-isneg (m)
  "This functions return t if all the elements of the matrix m are strictly negative,
and nil otherwise."
  (= (gsl-matrix-isneg m) 1))

(defun matrix-isnonneg (m)
  "This functions return t if all the elements of the matrix m are non-negative,
and nil otherwise."
  (= (gsl-matrix-isnonneg m) 1))

(defun matrix-equal (a b)
  "This function returns t if the matrices a and b are equal (by comparison of element
values) and nil otherwise."
  (and (= (matrix-row-size a) (matrix-row-size b))
       (= (matrix-column-size a) (matrix-column-size b))
       (= (gsl-matrix-equal a b) 1)))

(defun last1 (lst)
  (car (last lst)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun matrix-set-sequence (m seq)
  "This function sets each element of the matrix m to each element of the sequence
seq respectively."
  (let ((row (matrix-row-size m))
        (col (matrix-column-size m)))
    (if (not (= (* row col) (length seq)))
        (error "sequence length must be matrix size (size1 * size2)")
        (let ((idx 0))
          (dotimes (i row m)
            (dotimes (j col)
              (gsl-matrix-set m i j (coerce (elt seq idx) 'double-float))
              (setf idx (1+ idx))))))))

(defun matrix-set-2darray (m 2darray)
  "This function sets each element of the matrix m to each element of the 2 dimensions
array respectively."
  (let ((row (matrix-row-size m))
        (col (matrix-column-size m))
        (dim (array-dimensions 2darray)))
    (if (not (and (= row (first dim)) (= col (last1 dim))))
        (error "array dimension must be matrix dimension")
        (dotimes (i row m)
          (dotimes (j col)
            (gsl-matrix-set m i j (coerce (aref 2darray i j) 'double-float)))))))

;;; Allocate an aline of (struct gsl-matrix) in foeign heap, and return a pointer to it.
(defun make-matrix (n1 n2 &key (initial-element nil) (initial-contents nil))
  "This function creates a matrix of size n1 rows by n2 columns, returning a pointer
to a newly initialized matrix struct. A new block is allocated for the elements of
the matrix, and stored in the block component of the matrix struct. The block is
owned by the matrix, and will be deallocated when the matrix is deallocated.
The memory is allocated using gsl-matrix-alloc, so it can be passed to foreign
functions which gsl-matrix-free, or released using free-alien."
  (let ((m (make-alien (struct gsl-matrix))))
    (progn (setf m (matrix-calloc n1 n2))
           (cond ((not (null initial-element))
                  (matrix-set-all m initial-element))
                 ((not (null initial-contents))
                  (cond ((consp initial-contents)
                         (matrix-set-sequence m (flatten initial-contents)))
                        ((and (arrayp initial-contents)
                              (= (length (array-dimensions initial-contents)) 1))
                         (matrix-set-sequence m initial-contents))
                        ((and (arrayp initial-contents)
                              (= (length (array-dimensions initial-contents)) 2))
                         (matrix-set-2darray m initial-contents))
                        (t (error "initial-contents is unsupported type"))))
                 (t m)))))

(defun matrix-2darray (m)
  "This function return the 2darray whose elements is equal to elements of the matrix."
  (let ((acc (make-array (list (matrix-row-size m)
                               (matrix-column-size m))
                         :element-type 'double-float)))
    (dotimes (i (matrix-row-size m) acc)
      (dotimes (j (matrix-column-size m))
        (setf (aref acc i j) (matrix-get m i j))))))
