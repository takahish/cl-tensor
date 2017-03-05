;;;; scl/matrix.lisp

;;;; Copyright (C) 2016 Takahiro Ishikawa
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modif(loay it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see http://www.gnu.org/licenses/.

(cl:in-package "SCL")


;;; function

(defgeneric matrix-coerce (m element-type)
  (:documentation
   "Return a copy of matrix with elements coerced element type
element-type."))

(defmethod matrix-coerce ((m matrix-any) element-type)
  (let ((alt (make-matrix (size1 m) (size2 m) :element-type element-type))
        (etype (matrix-element-type element-type)))
    (dotimes (i (size1 m) alt)
      (dotimes (j (size2 m))
        (setf (aref (data alt) (+ (* i (tda alt)) j))
              (coerce (aref (data m) (+ (* i (tda m)) j)) etype))))))

(defgeneric matrix-get (m i j)
  (:documentation
   "This function return the (i,j)-th element of a matrix m. If i or j
lie outside the allowed range of 0 to n1-1 and 0 to n2-1 then the
error handler is invoked."))

(defmethod matrix-get ((m matrix-any) i j)
  (cond ((>= i (size1 m))
         (error "first index out of range"))
        ((>= j (size2 m))
         (error "second index out of range"))
        (t
         (aref (data m) (+ (* i (tda m)) j)))))

(defgeneric matrix-set (m i j x)
  (:documentation
   "This function stes the value of the (i,j)-th element of a matrix m
to x. If i or j liesoutside the allowed range of 0 to n1-1 and 0 to
n2-1 then the error handler is invoked."))

(defmethod matrix-set ((m matrix-any) i j x)
  (cond ((>= i (size1 m))
         (error "first index out of range"))
        ((>= j (size2 m))
         (error "second index out of range"))
        (t
         (setf (aref (data m) (+ (* i (tda m)) j)) x)))
  m)

(defgeneric matrix-set-all (m x)
  (:documentation
   "This function sets all the elements of the matrix m to the value
x."))

(defmethod matrix-set-all ((m matrix-any) x)
  (dotimes (i (size1 m) m)
    (dotimes (j (size2 m))
      (setf (aref (data m) (+ (* i (tda m)) j)) x))))

(defgeneric matrix-set-zero (m)
  (:documentation
   "This function sets all the elements of the matrix m to zero."))

(defmacro make-matrix-set-zero (element-type)
  (let ((mtype (matrix-type element-type))
        (zero (matrix-element-nil element-type)))
    `(defmethod matrix-set-zero ((m ,mtype))
       (dotimes (i (size1 m) m)
         (dotimes (j (size2 m))
           (setf (aref (data m) (+ (* i (tda m)) j)) ,zero))))))

(make-matrix-set-zero :any)

(make-matrix-set-zero :double)

(make-matrix-set-zero :float)

(make-matrix-set-zero :int)

(make-matrix-set-zero :uint)

(defgeneric matrix-set-identity (m)
  (:documentation
   "This function sets the elements of the matrix m to the
corresponding elements of the identity matrix m(i,j) = delta(i,j),
i.e. a unit diagonal with all off-diagonal elements zero. This applies
to both square and rectangular matrices."))

(defmacro make-matrix-set-identity (element-type)
  (let ((mtype (matrix-type element-type))
        (zero (matrix-element-nil element-type))
        (one (matrix-element-t element-type)))
    `(defmethod matrix-set-identity ((m ,mtype))
       (dotimes (i (size1 m) m)
         (dotimes (j (size2 m))
           (setf (aref (data m) (+ (* i (tda m)) j))
                 (if (= i j) ,one ,zero)))))))

(make-matrix-set-identity :any)

(make-matrix-set-identity :double)

(make-matrix-set-identity :float)

(make-matrix-set-identity :int)

(make-matrix-set-identity :uint)

(defgeneric matrix-submatrix (m i j n1 n2)
  (:documentation
   "This function return a matrix view of a submatrix of the matrix
m. The upper-left element of the submatrix is the element (i, j) of
the original matrix. The submatrix has n1 rows and n2 columns. The
physical number of columns in memory given by tda is unchanged."))

(defmacro make-matrix-submatrix (element-type)
  (let ((mtype (matrix-type element-type))
        (vwtype (matrix-view-type element-type))
        (etype (matrix-element-type element-type)))
    `(defmethod matrix-submatrix ((m ,mtype) i j n1 n2)
       (cond
         ((>= i (size1 m))
          (error "row index is out of range"))
         ((>= j (size2 m))
          (error "column index is out of range"))
         ((<= n1 0)
          (error "first dimension must be non-zero"))
         ((<= n2 0)
          (error "second dimension must be non-zero"))
         ((> (+ i n1) (size1 m))
          (error "first dimension overflows matrix"))
         ((> (+ j n2) (size2 m))
          (error "second dimension overflow matrix"))
         (t
          (let ((view (make-instance (quote ,vwtype)))
                (sub (make-instance (quote ,mtype))))
            ;; set sub
            (setf (data sub) (make-array (- (* (size1 m) (size2 m))
                                            (+ (* i (tda m)) j))
                                         :displaced-to (data m)
                                         :displaced-index-offset (+ (* i (tda m)) j)
                                         :element-type (quote ,etype))
                  (size1 sub) n1
                  (size2 sub) n2
                  (tda sub) (tda m)
                  (owner sub) nil)
            ;; set view
            (setf (shared-matrix view) sub)
            view))))))

(make-matrix-submatrix :any)

(make-matrix-submatrix :double)

(make-matrix-submatrix :float)

(make-matrix-submatrix :int)

(make-matrix-submatrix :uint)

(defgeneric matrix-view-vector (v n1 n2)
  (:documentation
   "This function return a matrix view of the vector v. The matrix
has n1 rows and n2 columns. The vector must have unit stride. The
physical number of columns in memory is also given by n2."))

(defmacro make-matrix-view-vector (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (matrix-view-type element-type))
        (etype (matrix-element-type element-type)))
    `(defmethod matrix-view-vector ((v ,vtype) n1 n2)
       (cond
         ((<= n1 0)
          (error "matrix dimension n1 must be positive integer"))
         ((<= n2 0)
          (error "matrix dimension n2 must be positive integer"))
         ((not (= (stride v) 1))
          (error "vector must have unit stride"))
         ((> (* n1 n2) (size v))
          (error "matrix size exceeds size of original"))
         (t
          (let ((view (make-instance (quote ,vwtype)))
                (m (make-instance (quote ,mtype))))
            ;; set m
            (setf (data m) (make-array (* n1 n2)
                                       :displaced-to (data v)
                                       :displaced-index-offset 0
                                       :element-type (quote ,etype))
                  (size1 m) n1
                  (size2 m) n2
                  (tda m) n2
                  (owner m) nil)
            ;; set view
            (setf (shared-matrix view) m)
            view))))))

(make-matrix-view-vector :any)

(make-matrix-view-vector :double)

(make-matrix-view-vector :float)

(make-matrix-view-vector :int)

(make-matrix-view-vector :uint)

(defun matrix-view-array (base n1 n2 &key (element-type :any))
  "This function return a matrix view of the array base. The matrix
has n1 rows and n2 columns."
  (cond
    ((<= n1 0)
     (error "matrix dimension n1 must be positive integer"))
    ((<= n2 0)
     (error "matrix dimension n2 must be positive integer"))
    ((not (typep base (matrix-data-type element-type n1 n2)))
     (error "data type mismatch"))
    (t
     (let ((view (make-instance (matrix-view-type element-type)))
           (m (make-instance (matrix-type element-type))))
       (setf (data m) base
             (size1 m) n1
             (size2 m) n2
             (tda m) n2
             (owner m) nil)
       (setf (shared-matrix view) m)
       view))))

(defgeneric matrix-row (m i)
  (:documentation
   "This function returns a vector view of the i-th row of the matrix
m. The data pointer of the new vector is set to null if i is out of
range."))

(defmacro make-matrix-row (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-row ((m ,mtype) i)
       (if (>= i (size1 m))
           (error "row index is out of range")
           (let ((view (make-instance (quote ,vwtype)))
                 (row (make-instance (quote ,vtype))))
             ;; set row
             (setf (data row) (make-array (size2 m)
                                          :displaced-to (data m)
                                          :displaced-index-offset (* i (tda m))
                                          :element-type (quote ,etype))
                   (size row) (size2 m)
                   (stride row) 1
                   (owner row) nil)
             ;; set view
             (setf (shared-vector view) row)
             view)))))

(make-matrix-row :any)

(make-matrix-row :double)

(make-matrix-row :float)

(make-matrix-row :int)

(make-matrix-row :uint)

(defgeneric matrix-column (m j)
  (:documentation
   "This function returns a vector view of the j-th column of the
matrix m. The data pointer of the new vector is set to null if j is
out of range."))

(defmacro make-matrix-column (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-column ((m ,mtype) j)
       (if (>= j (size2 m))
           (error "column index is out of range")
           (let ((view (make-instance (quote ,vwtype)))
                 (column (make-instance (quote ,vtype))))
             ;; set column
             (setf (data column) (make-array (- (* (size1 m) (size2 m)) j)
                                             :displaced-to (data m)
                                             :displaced-index-offset j
                                             :element-type (quote ,etype))
                   (size column) (size1 m)
                   (stride column) (tda m)
                   (owner column) nil)
             ;; set view
             (setf (shared-vector view) column)
             view)))))

(make-matrix-column :any)

(make-matrix-column :double)

(make-matrix-column :float)

(make-matrix-column :int)

(make-matrix-column :uint)

(defgeneric matrix-subrow (m i offset n)
  (:documentation
   "This function return a vector view of the i-th row of the matrix m
beginning at offset elements past the first column and containing n
elements."))

(defmacro make-matrix-subrow (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-subrow ((m ,mtype) i offset n)
       (cond
         ((>= i (size1 m))
          (error "row index is out of range"))
         ((<= n 0)
          (error "vector length n must be positive integer"))
         ((> (+ offset n) (size2 m))
          (error "dimension n overflows matrix"))
         (t
          (let ((view (make-instance (quote ,vwtype)))
                (subrow (make-instance (quote ,vtype))))
            ;; set subrow
            (setf (data subrow) (make-array n
                                            :displaced-to (data m)
                                            :displaced-index-offset (+ (* i (tda m)) offset)
                                            :element-type (quote ,etype))
                  (size subrow) n
                  (stride subrow) 1
                  (owner subrow) nil)
            ;; set view
            (setf (shared-vector view) subrow)
            view))))))

(make-matrix-subrow :any)

(make-matrix-subrow :double)

(make-matrix-subrow :float)

(make-matrix-subrow :int)

(make-matrix-subrow :uint)

(defgeneric matrix-subcolumn (m j offset n)
  (:documentation
   "This function return a vector view of the j-th column of the
matrix m beginning at offset elements past the first row and
containing n elements."))

(defmacro make-matrix-subcolumn (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-subcolumn ((m ,mtype) j offset n)
       (cond
         ((>= j (size1 m))
          (error "row index is out of range"))
         ((<= n 0)
          (error "vector length n must be positive integer"))
         ((> (+ offset n) (size2 m))
          (error "dimension n overflows matrix"))
         (t
          (let ((view (make-instance (quote ,vwtype)))
                (subcolumn (make-instance (quote ,vtype))))
            ;; set subcolumn
            (setf (data subcolumn) (make-array (- (* (size1 m) (size2 m))
                                                  (+ (* offset (tda m)) j))
                                               :displaced-to (data m)
                                               :displaced-index-offset (+ (* offset (tda m)) j)
                                               :element-type (quote ,etype))
                  (size subcolumn) n
                  (stride subcolumn) (tda m)
                  (owner subcolumn) nil)
            ;; set view
            (setf (shared-vector view) subcolumn)
            view))))))

(make-matrix-subcolumn :any)

(make-matrix-subcolumn :double)

(make-matrix-subcolumn :float)

(make-matrix-subcolumn :int)

(make-matrix-subcolumn :uint)

(defgeneric matrix-diagonal (m)
  (:documentation
   "This function return a vector view of the diagonal of the matrix
m. The matrix m is not required to be square. For a rectangular matrix
the length of the diagonal is the same as the smaller dimension of the
matrix."))

(defmacro make-matrix-diagonal (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-diagonal ((m ,mtype))
       (let ((view (make-instance (quote ,vwtype)))
             (diagonal (make-instance (quote ,vtype))))
         ;; set diagonal
         (setf (data diagonal) (make-array (* (size1 m) (size2 m))
                                           :displaced-to (data m)
                                           :displaced-index-offset 0
                                           :element-type (quote ,etype))
               (size diagonal) (min (size1 m) (size2 m))
               (stride diagonal) (1+ (tda m))
               (owner diagonal) nil)
         ;; set view
         (setf (shared-vector view) diagonal)
         view))))

(make-matrix-diagonal :any)

(make-matrix-diagonal :double)

(make-matrix-diagonal :float)

(make-matrix-diagonal :int)

(make-matrix-diagonal :uint)

(defgeneric matrix-subdiagonal (m k)
  (:documentation
   "These functions return a vector view of the k-th subdiagonal of
the matrix m. The matrix m is not required to be square. The diagonal
of the matrix corresponds to k = 0."))

(defmacro make-matrix-subdiagonal (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-subdiagonal ((m ,mtype) k)
       (if (>= k (size1 m))
           (error "subdiagonal index is out of range")
           (let ((view (make-instance (quote ,vwtype)))
                 (sub (make-instance (quote ,vtype))))
             ;; set subdiagonal
             (setf (data sub) (make-array (- (* (size1 m) (size2 m))
                                             (* k (tda m)))
                                          :displaced-to (data m)
                                          :displaced-index-offset (* k (tda m))
                                          :element-type (quote ,etype))
                   (size sub) (min (- (size1 m) k) (size2 m))
                   (stride sub) (1+ (tda m))
                   (owner sub) nil)
             ;; set view
             (setf (shared-vector view) sub)
             view)))))

(make-matrix-subdiagonal :any)

(make-matrix-subdiagonal :double)

(make-matrix-subdiagonal :float)

(make-matrix-subdiagonal :int)

(make-matrix-subdiagonal :uint)

(defgeneric matrix-superdiagonal (m k)
  (:documentation
   "Ths function return a vector view of the k-th superdiagonal of the
matrix m. The matrix m is not required to be square. The diagonal of
the matrix corresponds to k = 0."))

(defmacro make-matrix-superdiagonal (element-type)
  (let ((vtype (vector-type element-type))
        (mtype (matrix-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod matrix-superdiagonal ((m ,mtype) k)
       (if (>= k (size2 m))
           (error "column index is out of range")
           (let ((view (make-instance (quote ,vwtype)))
                 (super (make-instance (quote ,vtype))))
             ;; set superdiagonal
             (setf (data super) (make-array (- (* (size1 m) (size2 m)) k)
                                            :displaced-to (data m)
                                            :displaced-index-offset k
                                            :element-type (quote ,etype))
                   (size super) (min (size1 m) (- (size2 m) k))
                   (stride super) (1+ (tda m))
                   (owner super) nil)
             ;; set view
             (setf (shared-vector view) super)
             view)))))

(make-matrix-superdiagonal :any)

(make-matrix-superdiagonal :double)

(make-matrix-superdiagonal :float)

(make-matrix-superdiagonal :int)

(make-matrix-superdiagonal :uint)

(defgeneric matrix-copy (dest src)
  (:documentation
   "This function copies the elements of the matrix src into the
matrix dest. The two matrices must have the same size."))

(defmethod matrix-copy ((dest matrix-any) (src matrix-any))
  (if (or (not (= (size1 src) (size1 dest)))
          (not (= (size2 src) (size2 dest))))
      (error "matrix sizes are different")
      (dotimes (i (size1 src) dest)
        (dotimes (j (size2 src))
          (setf (aref (data dest) (+ (* i (tda dest)) j))
                (aref (data src) (+ (* i (tda src)) j)))))))

(defgeneric matrix-get-row (v m i)
  (:documentation
   "This function copies the elements of the i-th row of the matrix m
into the vector v. The length of the vector must be the same as the
length of the row."))

(defmethod matrix-get-row ((v vector-any) (m matrix-any) i)
  (cond
    ((>= i (size1 m))
     (error "row index is out of range"))
    ((not (= (size v) (size2 m)))
     (error "matrix row size and vector length are not equal"))
    (t
     (dotimes (j (size2 m) v)
       (setf (aref (data v) (* (stride v) j))
             (aref (data m) (+ (* i (tda m)) j)))))))

(defgeneric matrix-set-row (m i v)
  (:documentation
   "This function copies the elements of the vector v into the i-th
row of the matrix m. The length of the vector must be the same as the
length of the row."))

(defmethod matrix-set-row ((m matrix-any) i (v vector-any))
  (cond
    ((>= i (size1 m))
     (error "row index is out of range"))
    ((not (= (size v) (size2 m)))
     (error "matrix row size and vector length are not equal"))
    (t
     (dotimes (j (size2 m) m)
       (setf (aref (data m) (+ (* i (tda m)) j))
             (aref (data v) (* (stride v) j)))))))

(defgeneric matrix-get-col (v m j)
  (:documentation
   "This function copies the elements of the j-th column of the matrix
m into the vector v. The length of the vector must be the same as the
length of the column."))

(defmethod matrix-get-col ((v vector-any) (m matrix-any) j)
  (cond
    ((>= j (size2 m))
     (error "column index is out of range"))
    ((not (= (size v) (size1 m)))
     (error "matrix column size and vector length are not equal"))
    (t
     (dotimes (i (size1 m) v)
       (setf (aref (data v) (* (stride v) i))
             (aref (data m) (+ (* i (tda m)) j)))))))

(defgeneric matrix-set-col (m j v)
  (:documentation
   "This function copies the elements of the vector v into the j-th
column of the matrix m. The length of the vector must be the same
as the length of the column."))

(defmethod matrix-set-col ((m matrix-any) j (v vector-any))
  (cond
    ((>= j (size2 m))
     (error "column index is out of range"))
    ((not (= (size v) (size1 m)))
     (error "matrix column size and vector length are not equal"))
    (t
     (dotimes (i (size1 m) m)
       (setf (aref (data m) (+ (* i (tda m)) j))
             (aref (data v) (* (stride v) i)))))))

(defgeneric matrix-swap-rows (m i j)
  (:documentation
   "This function exchanges the i-th and j-th rows of the matrix m
in-place."))

(defmethod matrix-swap-rows ((m matrix-any) i j)
  (cond
    ((>= i (size1 m))
     (error "first row index is out of range"))
    ((>= j (size1 m))
     (error "second row index is out of range"))
    (t
     (if (not (= i j))
         (dotimes (k (size2 m) m)
           (rotatef (aref (data m) (+ (* i (tda m)) k))
                    (aref (data m) (+ (* j (tda m)) k))))
         m))))

(defgeneric matrix-swap-columns (m i j)
  (:documentation
   "This function exchanges the i-th and j-th columns of the matrix m
in-place."))

(defmethod matrix-swap-columns ((m matrix-any) i j)
  (cond
    ((>= i (size2 m))
     (error "first column index is out of range"))
    ((>= j (size2 m))
     (error "second column index is out of range"))
    (t
     (if (not (= i j))
         (dotimes (k (size1 m) m)
           (rotatef (aref (data m) (+ (* k (tda m)) i))
                    (aref (data m) (+ (* k (tda m)) j))))
         m))))

(defgeneric matrix-transpose (dest src)
  (:documentation
   "This function makes the matrix dest the transpose of the matrix
src by copying the elements of src into dest. This function works for
all matrices provided that the dimensions of the matrix dest match the
transposed dimensions of the matrix src."))

(defmethod matrix-transpose ((dest matrix-any) (src matrix-any))
  (if (or (not (= (size2 dest) (size1 src)))
          (not (= (size1 dest) (size2 src))))
      (error "dimensions of dext matrix must be transpose of rsc matrix")
      (dotimes (i (size1 dest) dest)
        (dotimes (j (size2 dest))
          (setf (aref (data dest) (+ (* i (tda dest)) j))
                (aref (data src) (+ (* j (tda src)) i)))))))

(defgeneric matrix-add (a b)
  (:documentation
   "This function adds the elements of matrix b to the elements of
matrix a. The result a(i, j) <- a(i, j) + b(i, j) is stored in a and b
remains unchanged. The two matrices must have the same dimensions."))

(defmethod matrix-add ((a matrix-any) (b matrix-any))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (+ (aref (data a) (+ (* i (tda a)) j))
                   (aref (data b) (+ (* i (tda b)) j))))))))

(defgeneric matrix-sub (a b)
  (:documentation
   "This function subtracts the elements of matrix b from the elements
of matrix a. The result a(i, j) <- a(i, j) − b(i, j) is stored in a
and b remains unchanged. The two matrices must have the same
dimensions."))

(defmethod matrix-sub ((a matrix-any) (b matrix-any))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (- (aref (data a) (+ (* i (tda a)) j))
                   (aref (data b) (+ (* i (tda b)) j))))))))

(defgeneric matrix-mul-elements (a b)
  (:documentation
   "This function multiplies the elements of matrix a by the elements
of matrix b. The result a(i, j) <- a(i, j) ∗ b(i, j) is stored in a and
b remains unchanged. The two matrices must have the same dimensions."))

(defmethod matrix-mul-elements ((a matrix-any) (b matrix-any))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (* (aref (data a) (+ (* i (tda a)) j))
                   (aref (data b) (+ (* i (tda b)) j))))))))

(defgeneric matrix-div-elements (a b)
  (:documentation
   "This function divides the elements of matrix a by the elements of
matrix b. The result a(i, j) <- a(i, j)/b(i, j) is stored in a and b
remains unchanged. The two matrices must have the same dimensions."))

(defmethod matrix-div-elements ((a matrix-any) (b matrix-any))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (/ (aref (data a) (+ (* i (tda a)) j))
                   (aref (data b) (+ (* i (tda b)) j))))))))

(defgeneric matrix-scale (a x)
  (:documentation
   "This function multiplies the elements of matrix a by the constant
factor x. The result a(i, j) <- xa(i, j) is stored in a."))

(defmethod matrix-scale ((a matrix-any) x)
  (dotimes (i (size1 a) a)
    (dotimes (j (size2 a))
      (setf (aref (data a) (+ (* i (tda a)) j))
            (* (aref (data a) (+ (* i (tda a)) j)) x)))))

(defgeneric matrix-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
matrix a. The result a(i, j) <- a(i, j) + x is stored in a."))

(defmethod matrix-add-constant ((a matrix-any) x)
  (dotimes (i (size1 a) a)
    (dotimes (j (size2 a))
      (setf (aref (data a) (+ (* i (tda a)) j))
            (+ (aref (data a) (+ (* i (tda a)) j)) x)))))

(defgeneric matrix-max (m)
  (:documentation
   "This function returns the maximum value in the matrix m."))

(defmethod matrix-max ((m matrix-any))
  (reduce #'max (data m)))

(defgeneric matrix-min (m)
  (:documentation
   "This function returns the minimum value in the matrix m."))

(defmethod matrix-min ((m matrix-any))
  (reduce #'min (data m)))

(defgeneric matrix-minmax (m)
  (:documentation
   "This function returns the minimum and maximum values in the matrix
 m, storing them in min out and max out."))

(defmethod matrix-minmax ((m matrix-any))
  (values (reduce #'min (data m)) (reduce #'max (data m))))

(defgeneric matrix-max-index (m)
  (:documentation
   "This function returns the indices of the maximum value in the
matrix m, storing them in imax and jmax. When there are several equal
maximum elements then the first element found is returned, searching
in row-major order."))

(defmethod matrix-max-index ((m matrix-any))
  (truncate (position (reduce #'max (data m)) (data m)) (tda m)))

(defgeneric matrix-min-index (m)
  (:documentation
   "This function returns the indices of the minimum value in the
matrix m, storing them in imin and jmin. When there are several
equal minimum elements then the first element found is returned,
searching in row-major order."))

(defmethod matrix-max-index ((m matrix-any))
  (truncate (position (reduce #'min (data m)) (data m)) (tda m)))

(defgeneric matrix-minmax-index (m)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the matrix m, storing them in (imin,jmin)
and (imax,jmax). When there are several equal min- imum or maximum
elements then the first elements found are returned, searching in
row-major order."))

(defmethod matrix-minmax-index ((m matrix-any))
  (multiple-value-bind (imin jmin)
      (truncate (position (reduce #'min (data m)) (data m)) (tda m))
    (multiple-value-bind (imax jmax)
        (truncate (position (reduce #'max (data m)) (data m)) (tda m))
      (values imin jmin imax jmax))))

;; numeric element-type only
(defgeneric matrix-isnull (m)
  (:documentation
   "Ths function returns t if all the elements of the matrix m are
zero, and nil otherwise."))

(defmacro make-matrix-isnull (element-type)
  (let ((mtype (matrix-type element-type))
        (zero (matrix-element-nil element-type)))
    `(defmethod matrix-isnull ((m ,mtype))
       (block test
         (dotimes (i (size1 m) t)
           (dotimes (j (size2 m))
             (if (not (= (aref (data m) (+ (* i (tda m)) j)) ,zero))
                 (return-from test nil))))))))

(make-matrix-isnull :double)

(make-matrix-isnull :float)

(make-matrix-isnull :int)

(make-matrix-isnull :uint)

;; numeric variable type only
(defgeneric matrix-ispos (m)
  (:documentation
   "This function returns t if all the elements of the matrix m are
strictly positive, and nil otherwise."))

(defmacro make-matrix-ispos (element-type)
  (let ((mtype (matrix-type element-type))
        (zero (matrix-element-nil element-type)))
    `(defmethod matrix-ispos ((m ,mtype))
       (block test
         (dotimes (i (size1 m) t)
           (dotimes (j (size2 m))
             (if (<= (aref (data m) (+ (* i (tda m)) j)) ,zero)
                 (return-from test nil))))))))

(make-matrix-ispos :double)

(make-matrix-ispos :float)

(make-matrix-ispos :int)

(make-matrix-ispos :uint)

;; numeric variable type only
(defgeneric matrix-isneg (m)
  (:documentation
   "This function returns t if all the elements of the matrix m are
strictly negative, and nil otherwise."))

(defmacro make-matrix-isneg (element-type)
  (let ((mtype (matrix-type element-type))
        (zero (matrix-element-nil element-type)))
    `(defmethod matrix-isneg ((m ,mtype))
       (block test
         (dotimes (i (size1 m) t)
           (dotimes (j (size2 m))
             (if (>= (aref (data m) (+ (* i (tda m)) j)) ,zero)
                 (return-from test  nil))))))))

(make-matrix-isneg :double)

(make-matrix-isneg :float)

(make-matrix-isneg :int)

(make-matrix-isneg :uint)

;; numeric variable type only
(defgeneric matrix-isnonneg (m)
  (:documentation
   "This function returns t if all the elements of the matrix m are
non-negative, and nil otherwise."))

(defmacro make-matrix-isnonneg (element-type)
  (let ((mtype (matrix-type element-type))
        (zero (matrix-element-nil element-type)))
    `(defmethod matrix-isnonneg ((m ,mtype))
       (block test
         (dotimes (i (size1 m) t)
           (dotimes (j (size2 m))
             (if (< (aref (data m) (+ (* i (tda m)) j)) ,zero)
                 (return-from test nil))))))))

(make-matrix-isnonneg :double)

(make-matrix-isnonneg :float)

(make-matrix-isnonneg :int)

(make-matrix-isnonneg :uint)

;; numeric variable type only
(defgeneric matrix-equal (a b)
  (:documentation
   "This function returns t if the matrices a and b are equal (by
comparison of element values) and nil otherwise."))

(defmacro make-matrix-equal (element-type)
  (let ((mtype (matrix-type element-type)))
    `(defmethod matrix-equal ((a ,mtype) (b ,mtype))
       (if (or (not (= (size1 a) (size1 b)))
               (not (= (size2 a) (size2 b))))
           (error "matrices must have same dimensions")
           (block test
             (dotimes (i (size1 a) t)
               (dotimes (j (size2 a))
                 (if (not (= (aref (data a) (+ (* i (tda a)) j))
                             (aref (data b) (+ (* i (tda b)) j))))
                     (return-from test nil)))))))))

(make-matrix-equal :double)

(make-matrix-equal :float)

(make-matrix-equal :int)

(make-matrix-equal :uint)

(defgeneric matrix-read (m &optional stream n1 n2)
  (:documentation
   "This function reads into the matrix m from the open stream stream
in binary format. The matrix m must be preallocated with the correct
dimensions since the function uses the size of m to determine how many
bytes to read."))

(defmethod matrix-read ((m matrix-any)
                        &optional (stream *standard-input*) (n1 nil) (n2 nil))
  (let ((s1 (if (null n1) (size1 m) n1))
        (s2 (if (null n2) (size2 m) n2)))
    (dotimes (i s1 m)
      (dotimes (j s2)
        (setf (aref (data m) (+ (* i (tda m)) j))
              (read stream))))))

(defgeneric matrix-write (m &optional stream n1 n2)
  (:documentation
   "This function writes the elements of the matrix m to the stream
stream."))

(defmacro make-matrix-write (element-type)
  (let ((mtype (matrix-type element-type)))
    `(defmethod matrix-write ((m ,mtype)
                              &optional (stream *standard-output*) (n1 nil) (n2 nil))
       (let ((s1 (if (null n1) (size1 m) n1))
             (s2 (if (null n2) (size2 m) n2)))
         (dotimes (i s1 m)
           (dotimes (j s2)
             (if (= j (1- s2))
                 (format stream "~S~%" (aref (data m) (+ (* i (tda m)) j)))
                 (format stream "~S~C" (aref (data m) (+ (* i (tda m)) j)) #\tab))))))))

(make-matrix-write :any)

(make-matrix-write :double)

(make-matrix-write :float)

(make-matrix-write :int)

(make-matrix-write :uint)

(defvar *print-object-matrix-size1* 10)

(defvar *print-object-matrix-size2* 10)

(defun print-matrix (m stream)
  (format stream "; ~A x ~A matrix~%" (size1 m) (size2 m))
  (cond ((and (<= (size1 m) *print-object-matrix-size1*)
              (<= (size2 m) *print-object-matrix-size2*))
         (matrix-write m stream))
        ((and (> (size1 m) *print-object-matrix-size1*)
              (<= (size2 m) *print-object-matrix-size2*))
         (matrix-write m stream *print-object-matrix-size1* (size2 m))
         (format stream "; omitted ~A rows~%" (- (size1 m) *print-object-matrix-size1*)))
        ((and (<= (size1 m) *print-object-matrix-size1*)
              (> (size2 m) *print-object-matrix-size2*))
         (matrix-write m stream (size1 m) *print-object-matrix-size2*)
         (format stream "; omitted ~A columns~%" (- (size2 m) *print-object-matrix-size2*)))
        ((and (> (size1 m) *print-object-matrix-size1*)
              (> (size2 m) *print-object-matrix-size2*))
         (matrix-write m stream *print-object-matrix-size1* *print-object-matrix-size2*)
         (format stream "; omitted ~A rows and ~A columns~%"
                 (- (size1 m) *print-object-matrix-size1*)
                 (- (size2 m) *print-object-matrix-size2*)))))

;; matrix-any print-object
(defmethod print-object ((m matrix-any) stream)
  (print-matrix m stream)
  (call-next-method))

;; matrix-any-view print-object
(defmethod print-object ((view matrix-any-view) stream)
  (print-matrix (shared-matrix view) stream)
  (call-next-method))
