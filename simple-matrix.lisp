;;;; cl-sct/simple-matrix.lisp

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

(cl:in-package "SCT")

;;; simple-matrix

(defclass simple-matrix-t ()
  ((data :accessor data :initarg :data)
   (size1 :accessor size1 :initarg :size1)
   (size2 :accessor size2 :initarg :size2)
   (tda :accessor tda :initarg :tda)
   (owner :accessor owner :initarg :owner)))

(defclass simple-matrix-double (simple-matrix-t) ())

(defclass simple-matrix-float (simple-matrix-t) ())

(defclass simple-matrix-int (simple-matrix-t) ())

(defclass simple-matrix-uint (simple-matrix-t) ())

(defun make-simple-matrix (n1 n2 &key (element-type :t)
                                   (initial-element nil)
                                   (initial-contents nil))
  (cond
    ;; default
    ((and (eql element-type :t)
          (not (null initial-element)))
     (make-instance 'simple-matrix-t
                    :data (make-array (* n1 n2)
                                      :initial-element initial-element)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((and (eql element-type :t)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-t
                    :data (make-array (* n1 n2)
                                      :initial-contents initial-contents)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((eql element-type :t)
     (make-instance 'simple-matrix-t
                    :data (make-array (* n1 n2)
                                      :initial-element nil)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ;; element type: double
    ((and (eql element-type :double)
          (not (null initial-element)))
     (make-instance 'simple-matrix-double
                    :data (make-array (* n1 n2)
                                      :element-type 'double-float
                                      :initial-element initial-element)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((and (eql element-type :double)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-double
                    :data (make-array (* n1 n2)
                                      :element-type 'double-float
                                      :initial-contents initial-contents)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((eql element-type :double)
     (make-instance 'simple-matrix-double
                    :data (make-array (* n1 n2)
                                      :element-type 'double-float
                                      :initial-element 0.0d0)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ;; element type: float
    ((and (eql element-type :float)
          (not (null initial-element)))
     (make-instance 'simple-matrix-float
                    :data (make-array (* n1 n2)
                                      :element-type 'single-float
                                      :initial-element initial-element)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((and (eql element-type :float)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-float
                    :data (make-array (* n1 n2)
                                      :element-type 'single-float
                                      :initial-contents initial-contents)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((eql element-type :float)
     (make-instance 'simple-matrix-float
                    :data (make-array (* n1 n2)
                                      :element-type 'single-float
                                      :initial-element 0.0)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ;; element type: int
    ((and (eql element-type :int)
          (not (null initial-element)))
     (make-instance 'simple-matrix-int
                    :data (make-array (* n1 n2)
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((and (eql element-type :init)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-int
                    :data (make-array (* n1 n2)
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((eql element-type :init)
     (make-instance 'simple-matrix-int
                    :data (make-array (* n1 n2)
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ;; element type: unsigned-int
    ((and (eql element-type :unsigned-int)
          (not (null initial-element)))
     (make-instance 'simple-matrix-uint
                    :data (make-array (* n1 n2)
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((and (eql element-type :unsigned-int)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-uint
                    :data (make-array (* n1 n2)
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((eql element-type :unsigned-int)
     (make-instance 'simple-matrix-uint
                    :data (make-array (* n1 n2)
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    (t (error "unsapported element type"))))

;;; simple-matrix-view

(defclass simple-matrix-t-view ()
  ((shared-matrix :accessor shared-matrix :initarg :shared-matrix)))

(defclass simple-matrix-double-view (simple-matrix-t-view) ())

(defclass simple-matrix-float-view (simple-matrix-t-view) ())

(defclass simple-matrix-int-view (simple-matrix-t-view) ())

(defclass simple-matrix-uint-view (simple-matrix-t-view) ())

;;; functions

(defgeneric simple-matrix-coerce (m element-type))

(defmethod simple-matrix-coerce ((m simple-matrix-t) element-type)
  (let ((alt (make-simple-matrix (size1 m) (size2 m)
                                 :element-type element-type)))
    (dotimes (i (size1 m) alt)
      (dotimes (j (size2 m))
        (setf (aref (data alt) (+ (* i (tda alt)) j))
              (aref (data m) (+ (* i (tda m)) j)))))))

(defgeneric simple-matrix-get (m i j)
  (:documentation
   "This function return the (i,j)-th element of a matrix m. If i or j
lie outside the allowed range of 0 to n1-1 and 0 to n2-1 then the
error handler is invoked."))

(defmethod simple-matrix-get ((m simple-matrix-t) i j)
  (cond ((>= i (size1 m))
         (error "first index out of range"))
        ((>= j (size2 m))
         (error "second index out of range"))
        (t
         (aref (data m) (+ (* i (tda m)) j)))))

(defgeneric simple-matrix-set (m i j x)
  (:documentation
   "This function stes the value of the (i,j)-th element of a matrix m
to x. If i or j liesoutside the allowed range of 0 to n1-1 and 0 to
n2-1 then the error handler is invoked."))

(defmethod simple-matrix-set ((m simple-matrix-t) i j x)
  (cond ((>= i (size1 m))
         (error "first index out of range"))
        ((>= j (size2 m))
         (error "second index out of range"))
        (t
         (setf (aref (data m) (+ (* i (tda m)) j)) x)))
  m)

(defgeneric simple-matrix-set-all (m x)
  (:documentation
   "This function sets all the elements of the matrix m to the value
x."))

(defmethod simple-matrix-set-all ((m simple-matrix-t) x)
  (dotimes (i (size1 m) m)
    (dotimes (j (size2 m))
      (setf (aref (data m) (+ (* i (tda m)) j)) x))))

(defgeneric simple-matrix-set-zero (m)
  (:documentation
   "This function sets all the elements of the matrix m to zero."))

(defmacro make-simple-matrix-set-zero (class zero)
  `(defmethod simple-matrix-set-zero ((m ,class))
     (dotimes (i (size1 m) m)
       (dotimes (j (size2 m))
         (setf (aref (data m) (+ (* i (tda m)) j)) ,zero)))))

(make-simple-matrix-set-zero simple-matrix-double nil)

(make-simple-matrix-set-zero simple-matrix-double 0.0d0)

(make-simple-matrix-set-zero simple-matrix-float 0.0)

(make-simple-matrix-set-zero simple-matrix-int 0)

(make-simple-matrix-set-zero simple-matrix-uint 0)

(defgeneric simple-matrix-set-identity (m)
  (:documentation
   "This function sets the elements of the matrix m to the
corresponding elements of the identity matrix m(i,j) = delta(i,j),
i.e. a unit diagonal with all off-diagonal elements zero. This applies
to both square and rectangular matrices."))

(defmacro make-simple-matrix-set-identity (class zero one)
  `(defmethod simple-matrix-set-identity ((m ,class))
     (dotimes (i (size1 m) m)
       (dotimes (j (size2 m))
         (setf (aref (data m) (+ (* i (tda m)) j))
               (if (= i j) ,one ,zero))))))

(make-simple-matrix-set-identity simple-matrix-double nil t)

(make-simple-matrix-set-identity simple-matrix-double 0.0d0 1.0d0)

(make-simple-matrix-set-identity simple-matrix-float 0.0 1.0)

(make-simple-matrix-set-identity simple-matrix-int 0 1)

(make-simple-matrix-set-identity simple-matrix-uint 0 1)

(defgeneric simple-matrix-submatrix (m i j n1 n2)
  (:documentation
   "This function return a matrix view of a submatrix of the matrix
m. The upper-left element of the submatrix is the element (i, j) of
the original matrix. The submatrix has n1 rows and n2 columns. The
physical number of columns in memory given by tda is unchanged."))

(defmacro make-simple-matrix-submatrix (matrix-class view-class)
  `(defmethod simple-matrix-submatrix ((m ,matrix-class) i j n1 n2)
     (cond
       ((or (<= i 0) (>= i (size1 m)))
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
        (let ((view (make-instance (quote ,view-class)))
              (sub (make-instance (quote ,matrix-class)))
              (element-type (second (type-of (data m)))))
          ;; set sub
          (setf (data sub) (make-array (* n1 n2)
                                       :displaced-to (data m)
                                       :displaced-index-offset (+ (* i (tda m)) j)
                                       :element-type element-type)
                (size1 sub) n1
                (size2 sub) n2
                (tda sub) (tda m)
                (owner sub) nil)
          ;; set view
          (setf (shared-matrix view) sub)
          view)))))

(make-simple-matrix-submatrix simple-matrix-t
                              simple-matrix-t-view)

(make-simple-matrix-submatrix simple-matrix-double
                              simple-matrix-double-view)

(make-simple-matrix-submatrix simple-matrix-float
                              simple-matrix-float-view)

(make-simple-matrix-submatrix simple-matrix-int
                              simple-matrix-int-view)

(make-simple-matrix-submatrix simple-matrix-uint
                              simple-matrix-uint-view)

(defgeneric simple-matrix-view-vector (v n1 n2)
  (:documentation
   "This function return a matrix view of the vector v. The matrix
has n1 rows and n2 columns. The vector must have unit stride. The
physical number of columns in memory is also given by n2."))

(defmacro make-simple-matrix-view-vector (vector-class
                                          matrix-class
                                          view-class)
  `(defmethod simple-matrix-view-vector ((v ,vector-class) n1 n2)
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
        (let ((view (make-instance (quote ,view-class)))
              (m (make-instance (quote ,matrix-class)))
              (element-type (second (type-of (data v)))))
          ;; set m
          (setf (data m) (make-array (* n1 n2)
                                     :displaced-to (data v)
                                     :displaced-index-offset 0
                                     :element-type element-type)
                (size1 m) n1
                (size2 m) n2
                (tda m) n2
                (owner m) nil)
          ;; set view
          (setf (shared-matrix view) m)
          view)))))

(make-simple-matrix-view-vector simple-vector-t
                                simple-matrix-t
                                simple-matrix-t-view)

(make-simple-matrix-view-vector simple-vector-double
                                simple-matrix-double
                                simple-matrix-double-view)

(make-simple-matrix-view-vector simple-vector-float
                                simple-matrix-float
                                simple-matrix-float-view)

(make-simple-matrix-view-vector simple-vector-int
                                simple-matrix-int
                                simple-matrix-int-view)

(make-simple-matrix-view-vector simple-vector-uint
                                simple-matrix-uint
                                simple-matrix-uint-view)

(defgeneric simple-matrix-row (m i)
  (:documentation
   "This function return a vector view of the i-th row of the matrix
m. The data pointer of the new vector is set to null if i is out of
range."))

(defmacro make-simple-matrix-row (vector-class
                                  matrix-class
                                  view-class)
  `(defmethod simple-matrix-row ((m ,matrix-class) i)
     (if (>= i (size1 m))
         (error "row index is out of range")
         (let ((view (make-instance (quote ,view-class)))
               (row (make-instance (quote ,vector-class)))
               (element-type (second (type-of (data m)))))
           ;; set row
           (setf (data row) (make-array (size2 m)
                                      :displaced-to (data m)
                                      :displaced-index-offset (* i (tda m))
                                      :element-type element-type)
                 (size row) (size2 m)
                 (stride row) 1
                 (owner row) nil)
           ;; set view
           (setf (shared-vector view) row)
           view))))

(make-simple-matrix-row simple-vector-t
                        simple-matrix-t
                        simple-vector-t-view)

(make-simple-matrix-row simple-vector-double
                        simple-matrix-double
                        simple-vector-double-view)

(make-simple-matrix-row simple-vector-float
                        simple-matrix-float
                        simple-vector-float-view)

(make-simple-matrix-row simple-vector-int
                        simple-matrix-int
                        simple-vector-int-view)

(make-simple-matrix-row simple-vector-uint
                        simple-matrix-uint
                        simple-vector-uint-view)

(defgeneric simple-matrix-column (m j)
  (:documentation
   "This function return a vector view of the j-th column of the
matrix m. The data pointer of the new vector is set to null if j is
out of range."))

(defmacro make-simple-matrix-column (vector-class
                                     matrix-class
                                     view-class)
  `(defmethod simple-matrix-column ((m ,matrix-class) j)
     (if (>= j (size2 m))
         (error "column index is out of range")
         (let ((view (make-instance (quote ,view-class)))
               (column (make-instance (quote ,vector-class)))
               (element-type (second (type-of (data m)))))
           ;; set column
           (setf (data column) (make-array (- (* (size1 m) (size2 m)) j)
                                           :displaced-to (data m)
                                           :displaced-index-offset j
                                           :element-type element-type)
                 (size column) (size1 m)
                 (stride column) (tda m)
                 (owner column) nil)
           ;; set view
           (setf (shared-vector view) column)
           view))))

(make-simple-matrix-column simple-vector-t
                           simple-matrix-t
                           simple-vector-t-view)

(make-simple-matrix-column simple-vector-double
                           simple-matrix-double
                           simple-vector-double-view)

(make-simple-matrix-column simple-vector-float
                           simple-matrix-float
                           simple-vector-float-view)

(make-simple-matrix-column simple-vector-int
                           simple-matrix-int
                           simple-vector-int-view)

(make-simple-matrix-column simple-vector-uint
                           simple-matrix-uint
                           simple-vector-uint-view)

(defgeneric simple-matrix-subrow (m i offset n)
  (:documentation
   "This function return a vector view of the i-th row of the matrix m
beginning at offset elements past the first column and containing n
elements."))

(defmacro make-simple-matrix-subrow (vector-class
                                     matrix-class
                                     view-class)
  `(defmethod simple-matrix-subrow ((m ,matrix-class) i offset n)
     (cond
       ((>= i (size1 m))
        (error "row index is out of range"))
       ((<= n 0)
        (error "vector length n must be positive integer"))
       ((> (+ offset n) (size2 m))
        (error "dimension n overflows matrix"))
       (t
        (let ((view (make-instance (quote ,view-class)))
              (subrow (make-instance (quote ,vector-class)))
              (element-type (second (type-of (data m)))))
          ;; set subrow
          (setf (data subrow) (make-array n
                                          :displaced-to (data m)
                                          :displaced-index-offset (+ (* i (tda m)) offset)
                                          :element-type element-type)
                (size subrow) n
                (stride subrow) 1
                (owner subrow) nil)
          ;; set view
          (setf (shared-vector view) subrow)
          view)))))

(make-simple-matrix-subrow simple-vector-t
                           simple-matrix-t
                           simple-vector-t-view)

(make-simple-matrix-subrow simple-vector-double
                           simple-matrix-double
                           simple-vector-double-view)

(make-simple-matrix-subrow simple-vector-float
                           simple-matrix-float
                           simple-vector-float-view)

(make-simple-matrix-subrow simple-vector-int
                           simple-matrix-int
                           simple-vector-int-view)

(make-simple-matrix-subrow simple-vector-uint
                           simple-matrix-uint
                           simple-vector-uint-view)

(defgeneric simple-matrix-subcolumn (m j offset n)
  (:documentation
   "This function return a vector view of the j-th column of the
matrix m beginning at offset elements past the first row and
containing n elements."))

(defmacro make-simple-matrix-subcolumn (vector-class
                                        matrix-class
                                        view-class)
  `(defmethod simple-matrix-subcolumn ((m ,matrix-class) j offset n)
     (cond
       ((>= j (size1 m))
        (error "row index is out of range"))
       ((<= n 0)
        (error "vector length n must be positive integer"))
       ((> (+ offset n) (size2 m))
        (error "dimension n overflows matrix"))
       (t
        (let ((view (make-instance (quote ,view-class)))
              (subcolumn (make-instance (quote ,vector-class)))
              (element-type (second (type-of (data m)))))
          ;; set subcolumn
          (setf (data subcolumn) (make-array (- (* (size1 m) (size2 m))
                                                (+ (* offset (tda m)) j))
                                             :displaced-to (data m)
                                             :displaced-index-offset (+ (* offset (tda m)) j)
                                             :element-type element-type)
                (size subcolumn) n
                (stride subcolumn) (tda m)
                (owner subcolumn) nil)
          ;; set view
          (setf (shared-vector view) subcolumn)
          view)))))

(make-simple-matrix-subcolumn simple-vector-t
                              simple-matrix-t
                              simple-vector-t-view)

(make-simple-matrix-subcolumn simple-vector-double
                              simple-matrix-double
                              simple-vector-double-view)

(make-simple-matrix-subcolumn simple-vector-float
                              simple-matrix-float
                              simple-vector-float-view)

(make-simple-matrix-subcolumn simple-vector-int
                              simple-matrix-int
                              simple-vector-int-view)

(make-simple-matrix-subcolumn simple-vector-uint
                              simple-matrix-uint
                              simple-vector-uint-view)

(defgeneric simple-matrix-diagonal (m)
  (:documentation
   "This function return a vector view of the diagonal of the matrix
m. The matrix m is not required to be square. For a rectangular matrix
the length of the diagonal is the same as the smaller dimension of the
matrix."))

(defmacro make-simple-matrix-diagonal (vector-class
                                       matrix-class
                                       view-class)
  `(defmethod simple-matrix-diagonal ((m ,matrix-class))
     (let ((view (make-instance (quote ,view-class)))
           (diagonal (make-instance (quote ,vector-class)))
           (element-type (second (type-of (data m)))))
       ;; set diagonal
       (setf (data diagonal) (make-array (* (size1 m) (size2 m))
                                         :displaced-to (data m)
                                         :displaced-index-offset 0
                                         :element-type element-type)
             (size diagonal) (min (size1 m) (size2 m))
             (stride diagonal) (+ (tda m) 1)
             (owner diagonal) nil)
       ;; set view
       (setf (shared-vector view) diagonal)
       view)))

(make-simple-matrix-diagonal simple-vector-t
                             simple-matrix-t
                             simple-vector-t-view)

(make-simple-matrix-diagonal simple-vector-double
                             simple-matrix-double
                             simple-vector-double-view)

(make-simple-matrix-diagonal simple-vector-float
                             simple-matrix-float
                             simple-vector-float-view)

(make-simple-matrix-diagonal simple-vector-int
                             simple-matrix-int
                             simple-vector-int-view)

(make-simple-matrix-diagonal simple-vector-uint
                             simple-matrix-uint
                             simple-vector-uint-view)

(defgeneric simple-matrix-subdiagonal (m k)
  (:documentation
   "These functions return a vector view of the k-th subdiagonal of
the matrix m. The matrix m is not required to be square. The diagonal
of the matrix corresponds to k = 0."))

(defmacro make-simple-matrix-subdiagonal (vector-class
                                          matrix-class
                                          view-class)
  `(defmethod simple-matrix-subdiagonal ((m ,matrix-class) k)
     (if (>= k (size1 m))
         (error "subdiagonal index is out of range")
         (let ((view (make-instance (quote ,view-class)))
               (sub (make-instance (quote ,vector-class)))
               (element-type (second (type-of (data m)))))
           ;; set subdiagonal
           (setf (data sub) (make-array (- (* (size1 m) (size2 m))
                                           (* k (tda m)))
                                        :displaced-to (data m)
                                        :displaced-index-offset (* k (tda m))
                                        :element-type element-type)
                 (size sub) (min (- (size1 m) k) (size2 m))
                 (stride sub) (+ (tda m) 1)
                 (owner sub) nil)
           ;; set view
           (setf (shared-vector view) sub)
           view))))

(make-simple-matrix-subdiagonal simple-vector-t
                                simple-matrix-t
                                simple-vector-t-view)

(make-simple-matrix-subdiagonal simple-vector-double
                                simple-matrix-double
                                simple-vector-double-view)

(make-simple-matrix-subdiagonal simple-vector-float
                                simple-matrix-float
                                simple-vector-float-view)

(make-simple-matrix-subdiagonal simple-vector-int
                                simple-matrix-int
                                simple-vector-int-view)

(make-simple-matrix-subdiagonal simple-vector-uint
                                simple-matrix-uint
                                simple-vector-uint-view)

(defgeneric simple-matrix-superdiagonal (m k)
  (:documentation
   "Ths function return a vector view of the k-th superdiagonal of the
matrix m. The matrix m is not required to be square. The diagonal of
the matrix corresponds to k = 0."))

(defmacro make-simple-matrix-superdiagonal (vector-class
                                            matrix-class
                                            view-class)
  `(defmethod simple-matrix-superdiagonal ((m ,matrix-class) k)
     (if (>= k (size2 m))
         (error "column index is out of range")
         (let ((view (make-instance (quote ,view-class)))
               (super (make-instance (quote ,vector-class)))
               (element-type (second (type-of (data m)))))
           ;; set superdiagonal
           (setf (data super) (make-array (- (* (size1 m) (size2 m)) k)
                                          :displaced-to (data m)
                                          :displaced-index-offset k
                                          :element-type element-type)
                 (size super) (min (size1 m) (- (size2 m) k))
                 (stride super) (+ (tda m) 1)
                 (owner super) nil)
           ;; set view
           (setf (shared-vector view) super)
           view))))

(make-simple-matrix-superdiagonal simple-vector-t
                                  simple-matrix-t
                                  simple-vector-t-view)

(make-simple-matrix-superdiagonal simple-vector-double
                                  simple-matrix-double
                                  simple-vector-double-view)

(make-simple-matrix-superdiagonal simple-vector-float
                                  simple-matrix-float
                                  simple-vector-float-view)

(make-simple-matrix-superdiagonal simple-vector-int
                                  simple-matrix-int
                                  simple-vector-int-view)

(make-simple-matrix-superdiagonal simple-vector-uint
                                  simple-matrix-uint
                                  simple-vector-uint-view)

(defgeneric simple-matrix-copy (dest src)
  (:documentation
   "This function copies the elements of the matrix src into the
matrix dest. The two matrices must have the same size."))

(defmethod simple-matrix-copy ((dest simple-matrix-t)
                               (src simple-matrix-t))
  (if (or (not (= (size1 src) (size1 dest)))
          (not (= (size2 src) (size2 dest))))
      (error "matrix sizes are different")
      (dotimes (i (size1 src) dest)
        (dotimes (j (size2 src))
          (setf (aref (data dest) (+ (* i (tda dest)) j))
                (aref (data src) (+ (* i (tda src)) j)))))))

(defgeneric simple-matrix-get-row (v m i)
  (:documentation
   "This function copies the elements of the i-th row of the matrix m
into the vector v. The length of the vector must be the same as the
length of the row."))

(defmethod simple-matrix-get-row ((v simple-vector-t)
                                  (m simple-matrix-t) i)
  (cond
    ((>= i (size1 m))
     (error "row index is out of range"))
    ((not (= (size v) (size2 m)))
     (error "matrix row size and vector length are not equal"))
    (t
     (dotimes (j (size2 m) v)
       (setf (aref (data v) (* (stride v) j))
             (aref (data m) (+ (* i (tda m)) j)))))))

(defgeneric simple-matrix-set-row (m i v)
  (:documentation
   "This function copies the elements of the vector v into the i-th
row of the matrix m. The length of the vector must be the same as the
length of the row."))

(defmethod simple-matrix-set-row ((m simple-matrix-t) i
                                  (v simple-vector-t))
  (cond
    ((>= i (size1 m))
     (error "row index is out of range"))
    ((not (= (size v) (size2 m)))
     (error "matrix row size and vector length are not equal"))
    (t
     (dotimes (j (size2 m) v)
       (setf (aref (data m) (+ (* i (tda m)) j))
             (aref (data v) (* (stride v) j)))))))

(defgeneric simple-matrix-get-col (v m j)
  (:documentation
   "This function copies the elements of the j-th column of the matrix
m into the vector v. The length of the vector must be the same as the
length of the column."))

(defmethod simple-matrix-get-col ((v simple-vector-t)
                                  (m simple-matrix-t) j)
  (cond
    ((>= j (size2 m))
     (error "column index is out of range"))
    ((not (= (size v) (size1 m)))
     (error "matrix column size and vector length are not equal"))
    (t
     (dotimes (i (size1 m) v)
       (setf (aref (data v) (* (stride v) i))
             (aref (data m) (+ (* i (tda m)) j)))))))

(defgeneric simple-matrix-set-col (m j v)
  (:documentation
   "This function copies the elements of the vector v into the j-th
column of the matrix m. The length of the vector must be the same
as the length of the column."))

(defmethod simple-matrix-set-col ((m simple-matrix-t) j
                                  (v simple-vector-t))
  (cond
    ((>= j (size2 m))
     (error "column index is out of range"))
    ((not (= (size v) (size1 m)))
     (error "matrix column size and vector length are not equal"))
    (t
     (dotimes (i (size1 m) v)
       (setf (aref (data m) (+ (* i (tda m)) j))
             (aref (data v) (* (stride v) i)))))))

(defgeneric simple-matrix-swap-rows (m i j)
  (:documentation
   "This function exchanges the i-th and j-th rows of the matrix m
in-place."))

(defmethod simple-matrix-swap-rows ((m simple-matrix-t) i j)
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

(defgeneric simple-matrix-swap-columns (m i j)
  (:documentation
   "This function exchanges the i-th and j-th columns of the matrix m
in-place."))

(defmethod simple-matrix-swap-columns ((m simple-matrix-t) i j)
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

(defgeneric simple-matrix-transpose (dest src)
  (:documentation
   "This function makes the matrix dest the transpose of the matrix
src by copying the elements of src into dest. This function works for
all matrices provided that the dimensions of the matrix dest match the
transposed dimensions of the matrix src."))

(defmethod simple-matrix-transpose ((dest simple-matrix-t)
                                    (src simple-matrix-t))
  (if (or (not (= (size2 dest) (size1 src)))
          (not (= (size1 dest) (size2 src))))
      (error "dimensions of dext matrix must be transpose of rsc matrix")
      (dotimes (i (size1 dest) dest)
        (dotimes (j (size2 dest))
          (setf (aref (data dest) (+ (* i (tda dest)) j))
                (aref (data src) (+ (* j (tda src)) i)))))))

(defgeneric simple-matrix-add (a b)
  (:documentation
   "This function adds the elements of matrix b to the elements of
matrix a. The result a(i, j) <- a(i, j) + b(i, j) is stored in a and b
remains unchanged. The two matrices must have the same dimensions."))

(defmethod simple-matrix-add ((a simple-matrix-t) (b simple-matrix-t))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (+ (aref (data a) (+ (* (tda a)) j))
                   (aref (data b) (+ (* (tda b)) j))))))))

(defgeneric simple-matrix-sub (a b)
  (:documentation
   "This function subtracts the elements of matrix b from the elements
of matrix a. The result a(i, j) <- a(i, j) − b(i, j) is stored in a
and b remains unchanged. The two matrices must have the same
dimensions."))

(defmethod simple-matrix-sub ((a simple-matrix-t) (b simple-matrix-t))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (+ (aref (data a) (+ (* (tda a)) j))
                   (aref (data b) (+ (* (tda b)) j))))))))

(defgeneric simple-matrix-mul-elements (a b)
  (:documentation
   "This function multiplies the elements of matrix a by the elements
of matrix b. The result a(i, j) <- a(i, j) ∗ b(i, j) is stored in a and
b remains unchanged. The two matrices must have the same dimensions."))

(defmethod simple-matrix-mul-elements ((a simple-matrix-t) (b simple-matrix-t))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (* (aref (data a) (+ (* i (tda a)) j))
                   (aref (data b) (+ (* i (tda b)) j))))))))

(defgeneric simple-matrix-div-elements (a b)
  (:documentation
   "This function divides the elements of matrix a by the elements of
matrix b. The result a(i, j) <- a(i, j)/b(i, j) is stored in a and b
remains unchanged. The two matrices must have the same dimensions."))

(defmethod simple-matrix-div-elements ((a simple-matrix-t) (b simple-matrix-t))
  (if (or (not (= (size1 b) (size1 a)))
          (not (= (size2 b) (size2 a))))
      (error "matrices must have same dimensions")
      (dotimes (i (size1 a) a)
        (dotimes (j (size2 a))
          (setf (aref (data a) (+ (* i (tda a)) j))
                (* (aref (data a) (+ (* i (tda a)) j))
                   (aref (data b) (+ (* i (tda b)) j))))))))

(defgeneric simple-matrix-scale (a x)
  (:documentation
   "This function multiplies the elements of matrix a by the constant
factor x. The result a(i, j) <- xa(i, j) is stored in a."))

(defmethod simple-matrix-scale ((a simple-matrix-t) x)
  (dotimes (i (size1 a) a)
    (dotimes (j (size2 a))
      (setf (aref (data a) (+ (* i (tda a)) j))
            (* (aref (data a) (+ (* i (tda a)) j)) x)))))

(defgeneric simple-matrix-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
matrix a. The result a(i, j) <- a(i, j) + x is stored in a."))

(defmethod simple-matrix-add-constant ((a simple-matrix-t) x)
  (dotimes (i (size1 a) a)
    (dotimes (j (size2 a))
      (setf (aref (data a) (+ (* i (tda a)) j))
            (+ (aref (data a) (+ (* i (tda a)) j)) x)))))

(defgeneric simple-matrix-max (m)
  (:documentation
   "This function returns the maximum value in the matrix m."))

(defmethod simple-matrix-max ((m simple-matrix-t))
  (reduce #'max (data m)))

(defgeneric simple-matrix-min (m)
  (:documentation
   "This function returns the minimum value in the matrix m."))

(defmethod simple-matrix-min ((m simple-matrix-t))
  (reduce #'min (data m)))

(defgeneric simple-matrix-minmax (m)
  (:documentation
   "This function returns the minimum and maximum values in the matrix
 m, storing them in min out and max out."))

(defmethod simple-matrix-minmax ((m simple-matrix-t))
  (values (reduce #'min (data m)) (reduce #'max (data m))))

(defgeneric simple-matrix-max-index (m)
  (:documentation
   "This function returns the indices of the maximum value in the
matrix m, storing them in imax and jmax. When there are several equal
maximum elements then the first element found is returned, searching
in row-major order."))

(defmethod simple-matrix-max-index ((m simple-matrix-t))
  (truncate (position (reduce #'max (data m)) (data m)) (tda m)))

(defgeneric simple-matrix-min-index (m)
  (:documentation
   "This function returns the indices of the minimum value in the
matrix m, storing them in imin and jmin. When there are several
equal minimum elements then the first element found is returned,
searching in row-major order."))

(defmethod simple-matrix-max-index ((m simple-matrix-t))
  (truncate (position (reduce #'min (data m)) (data m)) (tda m)))

(defgeneric simple-matrix-minmax-index (m)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the matrix m, storing them in (imin,jmin)
and (imax,jmax). When there are several equal min- imum or maximum
elements then the first elements found are returned, searching in
row-major order."))

(defmethod simple-matrix-minmax-index ((m simple-matrix-t))
  (multiple-value-bind (imin jmin)
      (truncate (position (reduce #'min (data m)) (data m)) (tda m))
    (multiple-value-bind (imax jmax)
        (truncate (position (reduce #'max (data m)) (data m)) (tda m))
      (values imin jmin imax jmax))))

(defgeneric simple-matrix-isnull (m)
  (:documentation
   "Ths function returns t if all the elements of the matrix m are
zero, and nil otherwise."))

(defmethod simple-matrix-isnull ((m simple-matrix-t))
  (dotimes (i (size1 m) t)
    (dotimes (j (size2 m))
      (if (not (null (aref (data m) (+ (* i (tda m)) j))))
          (return nil)))))

(defmacro make-simple-matrix-isnull (class zero)
  `(defmethod simple-matrix-isnull ((m ,class))
     (dotimes (i (size1 m) t)
       (dotimes (j (size2 m))
         (if (not (= (aref (data m) (+ (* i (tda m)) j)) ,zero))
             (return nil))))))

(make-simple-matrix-isnull simple-matrix-double 0.0d0)

(make-simple-matrix-isnull simple-matrix-float 0.0)

(make-simple-matrix-isnull simple-matrix-int 0)

(make-simple-matrix-isnull simple-matrix-uint 0)

;; numeric variable type only
(defgeneric simple-matrix-ispos (m)
  (:documentation
   "This function returns t if all the elements of the matrix m are
strictly positive, and nil otherwise."))

(defmacro make-simple-matrix-ispos (class zero)
  `(defmethod simple-matrix-ispos ((m ,class))
     (dotimes (i (size1 m) t)
       (dotimes (j (size2 m))
         (if (<= (aref (data m) (+ (* i (tda m)) j)) ,zero)
             (return nil))))))

(make-simple-matrix-ispos simple-matrix-double 0.0d0)

(make-simple-matrix-ispos simple-matrix-float 0.0)

(make-simple-matrix-ispos simple-matrix-int 0)

(make-simple-matrix-ispos simple-matrix-uint 0)

;; numeric variable type only
(defgeneric simple-matrix-isneg (m)
  (:documentation
   "This function returns t if all the elements of the matrix m are
strictly negative, and nil otherwise."))

(defmacro make-simple-matrix-isneg (class zero)
  `(defmethod simple-matrix-isneg ((m ,class))
     (dotimes (i (size1 m) t)
       (dotimes (j (size2 m))
         (if (>= (aref (data m) (+ (* i (tda m)) j)) ,zero)
             (return nil))))))

(make-simple-matrix-isneg simple-matrix-double 0.0d0)

(make-simple-matrix-isneg simple-matrix-float 0.0)

(make-simple-matrix-isneg simple-matrix-int 0)

(make-simple-matrix-isneg simple-matrix-uint 0)

;; numeric variable type only
(defgeneric simple-matrix-isnonneg (m)
  (:documentation
   "This function returns t if all the elements of the matrix m are
non-negative, and nil otherwise."))

(defmacro make-simple-matrix-isnonneg (class zero)
  `(defmethod simple-matrix-isnonneg ((m ,class))
     (dotimes (i (size1 m) t)
       (dotimes (j (size2 m))
         (if (< (aref (data m) (+ (* i (tda m)) j)) ,zero)
             (return nil))))))

(make-simple-matrix-isnonneg simple-matrix-double 0.0d0)

(make-simple-matrix-isnonneg simple-matrix-float 0.0)

(make-simple-matrix-isnonneg simple-matrix-int 0)

(make-simple-matrix-isnonneg simple-matrix-uint 0)

;; numeric variable type only
(defgeneric simple-matrix-equal (a b)
  (:documentation
   "This function returns t if the matrices a and b are equal (by
comparison of element values) and nil otherwise."))

(defmacro make-simple-matrix-equal (class)
  `(defmethod simple-matrix-equal ((a ,class) (b ,class))
     (if (or (not (= (size1 a) (size1 b)))
             (not (= (size2 a) (size2 b))))
         (error "matrices must have same dimensions")
         (dotimes (i (size1 a) t)
           (dotimes (j (size2 a))
             (if (not (= (aref (data a) (+ (* i (tda a)) j))
                         (aref (data b) (+ (* i (tda b)) j))))
                 (return nil)))))))

(make-simple-matrix-equal simple-matrix-double)

(make-simple-matrix-equal simple-matrix-float)

(make-simple-matrix-equal simple-matrix-int)

(make-simple-matrix-equal simple-matrix-uint)

(defgeneric simple-matrix-read (m &optional str n1 n2)
  (:documentation
   "This function reads into the matrix m from the open stream str
in binary format. The matrix m must be preallocated with the
correct dimensions since the function uses the size of m to
determine how many bytes to read."))

(defmethod simple-matrix-read ((m simple-matrix-t)
                               &optional (str *standard-input*)
                                 (n1 nil) (n2 nil))
  (let ((s1 (if (null n1) (size1 m) n1))
        (s2 (if (null n2) (size2 m) n2)))
    (dotimes (i s1 m)
      (dotimes (j s2)
        (setf (aref (data m) (+ (* i (tda m)) j))
              (read str))))))

(defgeneric simple-matrix-write (m &optional str n1 n2)
  (:documentation
   "This function writes the elements of the matrix m to the
stream."))

(defmacro make-simple-matrix-write (class element-type)
  `(defmethod simple-matrix-write ((m ,class)
                                  &optional (str *standard-output*)
                                    (n1 nil) (n2 nil))
     (let ((s1 (if (null n1) (size1 m) n1))
           (s2 (if (null n2) (size2 m) n2)))
       (format str "; ~A X ~A ~A SIMPLE-MATRIX~%" s1 s2 ,element-type)
       (dotimes (i s1 m)
         (dotimes (j s2)
           (if (= j (- s2 1))
               (format str "~S~%" (aref (data m) (+ (* i (tda m)) j)))
               (format str "~S~C" (aref (data m) (+ (* i (tda m)) j)) #\tab)))))))

(make-simple-matrix-write simple-matrix-t :t)

(make-simple-matrix-write simple-matrix-double :double)

(make-simple-matrix-write simple-matrix-float :float)

(make-simple-matrix-write simple-matrix-int :int)

(make-simple-matrix-write simple-matrix-uint :unsigned-int)
