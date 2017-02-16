;;;; cl-sct/vector.lisp

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


;;; element-type

(defvar *vector-element-type*
  `((:t . t)
    (:double . double-float)
    (:float . single-float)
    (:int . (signed-byte ,(* (cffi:foreign-type-size :int) 8)))
    (:uint . (unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))

(defun vector-element-type (element-type)
  (cdr (assoc element-type *vector-element-type*)))

(defvar *vector-element-nil*
  '((:t . nil)
    (:double . 0.0d0)
    (:float . 0.0)
    (:int . 0)
    (:uint . 0)))

(defun vector-element-nil (element-type)
  (cdr (assoc element-type *vector-element-nil*)))

(defvar *vector-element-t*
  '((:t . t)
    (:double . 1.0d0)
    (:float . 1.0)
    (:int . 1)
    (:uint . 1)))

(defun vector-element-t (element-type)
  (cdr (assoc element-type *vector-element-t*)))


;;; data type

(defun vector-data-type (element-type n)
  (cond
    ((eq element-type :t)
     `(simple-vector ,n))
    ((or (eq element-type :double)
         (eq element-type :float)
         (eq element-type :int)
         (eq element-type :uint))
     `(simple-array ,(vector-element-type element-type) (,n)))
    (t nil)))


;;; vector

(defclass vector-t ()
  ((data :accessor data :initarg :data)
   (size :accessor size :initarg :size)
   (stride :accessor stride :initarg :stride)
   (owner :accessor owner :initarg :owner)))

(defclass vector-double (vector-t) ())

(defclass vector-float (vector-t) ())

(defclass vector-int (vector-t) ())

(defclass vector-uint (vector-t) ())

(defvar *vector-type*
  '((:t . vector-t)
    (:double . vector-double)
    (:float . vector-float)
    (:int . vector-int)
    (:uint . vector-uint)))

(defun vector-type (element-type)
  (cdr (assoc element-type *vector-type*)))

(defun make-vector (n &key (element-type :t)
                        (initial-element nil) (initial-contents nil))
  (cond
    ((not (null initial-element))
     (make-instance (vector-type element-type)
                    :data (make-array n
                                      :element-type (vector-element-type element-type)
                                      :initial-element initial-element)
                    :size n
                    :stride 1
                    :owner t))
    ((not (null initial-contents))
     (make-instance (vector-type element-type)
                    :data (make-array n
                                      :element-type (vector-element-type element-type)
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1
                    :owner t))
    (t
     (make-instance (vector-type element-type)
                    :data (make-array n
                                      :element-type (vector-element-type element-type)
                                      :initial-element (vector-element-nil element-type))
                    :size n
                    :stride 1
                    :owner t))))


;;; vector-view

(defclass vector-t-view ()
  ((shared-vector :accessor shared-vector :initarg :shared-vector)))

(defclass vector-double-view (vector-t-view) ())

(defclass vector-float-view (vector-t-view) ())

(defclass vector-int-view (vector-t-view) ())

(defclass vector-uint-view (vector-t-view) ())

(defvar *vector-view-type*
  '((:t . vector-t-view)
    (:double . vector-double-view)
    (:float . vector-float-view)
    (:int . vector-int-view)
    (:uint . vector-uint-view)))

(defun vector-view-type (element-type)
  (cdr (assoc element-type *vector-view-type*)))


;;; functions

(defgeneric vector-coerce (v element-type)
  (:documentation
   "Return a copy of vector with elements coerced element type
element-type."))

(defmethod vector-coerce ((v vector-t) element-type)
  (let ((alt (make-vector (size v) :element-type element-type))
        (etype (vector-element-type element-type)))
    (dotimes (i (size v) alt)
      (setf (aref (data alt) (* i (stride alt)))
            (coerce (aref (data v) (* i (stride v))) etype)))))

(defgeneric vector-get (v i)
  (:documentation
   "This function retruns the i-th element of a vector v. If i lies
outside the allowed range of 0 to n - 1 then the error handler is
invoked."))

(defmethod vector-get ((v vector-t) i)
  ;; aref delegate range check.
  (aref (data v) (* i (stride v))))

(defgeneric vector-set (v i x)
  (:documentation
   "This function sets the value of the i-th element of a vector v to
x. If i lies outside the allowed range of 0 to n - 1 then the error
handler is invoked."))

(defmethod vector-set ((v vector-t) i x)
  ;; aref delegate range check.
  (setf (aref (data v) (* i (stride v))) x)
  v)

(defgeneric vector-set-all (v x)
  (:documentation
   "This function sets all the elements of the vector v to the value x."))

(defmethod vector-set-all ((v vector-t) x)
  (dotimes (i (size v) v)
    (setf (aref (data v) (* i (stride v))) x)))

(defgeneric vector-set-zero (v)
  (:documentation
   "This function sets all the elements of the vector v to zero."))

(defmacro make-vector-set-zero (element-type)
  (let ((vtype (vector-type element-type))
        (zero (vector-element-nil element-type)))
    `(defmethod vector-set-zero ((v ,vtype))
       (dotimes (i (size v) v)
         (setf (aref (data v) (* i (stride v))) ,zero)))))

(make-vector-set-zero :t)

(make-vector-set-zero :double)

(make-vector-set-zero :float)

(make-vector-set-zero :int)

(make-vector-set-zero :uint)

(defgeneric vector-set-basis (v i)
  (:documentation
   "This function makes a basis vector by setting all the elements of
the vector v to zero except for the i-th element which is set to one."))

(defmacro make-vector-set-basis (element-type)
  (let ((vtype (vector-type element-type))
        (zero (vector-element-nil element-type))
        (one (vector-element-t element-type)))
    `(defmethod vector-set-basis ((v ,vtype) i)
       (dotimes (j (size v))
         (setf (aref (data v) (* j (stride v))) ,zero))
       ;; aref delegate range check.
       (setf (aref (data v) (* i (stride v))) ,one)
       v)))

(make-vector-set-basis :t)

(make-vector-set-basis :double)

(make-vector-set-basis :float)

(make-vector-set-basis :int)

(make-vector-set-basis :uint)

(defgeneric vector-subvector (v offset n)
  (:documentation
   "This function return a vector view of a subvector of another
vector v. The start of the new vector is offset by offset elements
from the start of the original vector. The new vector has n
elements."))

(defmacro make-vector-subvector (element-type)
  (let ((vtype (vector-type element-type))
        (vwtype (vector-view-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod vector-subvector ((v ,vtype) offset n)
       (cond
         ((<= n 0)
          (error "vector length n must be positive integer"))
         ((>= (+ offset (1- n)) (size v))
          (error "view would extend past end of vector"))
         (t
          (let ((view (make-instance (quote ,vwtype)))
                (sub (make-instance (quote ,vtype))))
            ;; set sub
            (setf (data sub) (make-array n
                                         :displaced-to (data v)
                                         :displaced-index-offset offset
                                         :element-type (quote ,etype))
                  (size sub) n
                  (stride sub) (stride v)
                  (owner sub) nil)
            ;; set view
            (setf (shared-vector view) sub)
            view))))))

(make-vector-subvector :t)

(make-vector-subvector :double)

(make-vector-subvector :float)

(make-vector-subvector :int)

(make-vector-subvector :uint)

(defun vector-view-array (base n &key (element-type :t))
  "This function return a vector view of an array. The start of the
new vector is given by base and has n elements."
  (cond
    ((<= n 0)
     (error "vector length n must be positive integer"))
    ((not (typep base (vector-data-type element-type n)))
     (error "data type mismatch"))
    (t
     (let ((view (make-instance (vector-view-type element-type)))
           (v (make-instance (vector-type element-type))))
       (setf (data v) base
             (size v) n
             (stride v) 1
             (owner v) nil)
       (setf (shared-vector view) v)
       view))))

(defgeneric vector-copy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest. The two vectors must have the same length."))

(defmethod vector-copy ((dest vector-t) (src vector-t))
  (if (not (= (size dest) (size src)))
      (error "vector lengths are not equal")
      (dotimes (j (size src) dest)
        (setf (aref (data dest) (* (stride dest) j))
              (aref (data src) (* (stride src) j))))))

(defgeneric vector-swap-elements (v i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the vector v
in-place."))

(defmethod vector-swap-elements ((v vector-t) i j)
  ;; aref delegate range check.
  (if (not (= i j))
      (progn
        (rotatef (aref (data v) i) (aref (data v) j))
        v)
      v))

(defgeneric vector-reverse (v)
  (:documentation
   "This function reverses the order of the elements of the vector v."))

(defmethod vector-reverse ((v vector-t))
  ;; identity wrap up nreaverse, because STYLE-WARNING has occurred.
  (identity (nreverse (data v)))
  v)

(defgeneric vector-add (a b)
  (:documentation
   "This function adds the elements of vector b to the elements of
vector a. The result ai <- ai + bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod vector-add ((a vector-t) (b vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (+ (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements
of vector a. The result ai <- ai − bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod vector-sub ((a vector-t) (b vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (- (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements
of vector b. The result ai <- ai ∗ bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod vector-mul ((a vector-t) (b vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (* (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of
vector b. The result ai <- ai/bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod vector-div ((a vector-t) (b vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (/ (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric vector-scale (a x)
  (:documentation
   "This function multiplies the elements of vector a by the constant
factor x. The result ai <- xai is stored in a."))

(defmethod vector-scale ((a vector-t) x)
  (dotimes (i (size a) a)
    (setf (aref (data a) (* i (stride a)))
          (* (aref (data a) (* i (stride a))) x))))

(defgeneric vector-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
vector a. The result ai <- ai + x is stored in a."))

(defmethod vector-add-constant ((a vector-t) x)
  (dotimes (i (size a) a)
    (setf (aref (data a) (* i (stride a)))
          (+ (aref (data a) (* i (stride a))) x))))

(defgeneric vector-max (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmethod vector-max ((v vector-t))
  (reduce #'max (data v)))

(defgeneric vector-min (v)
  (:documentation
   "This function returns the minimum value in the vector v."))

(defmethod vector-min ((v vector-t))
  (reduce #'min (data v)))

(defgeneric vector-minmax (v)
  (:documentation
   "This function returns the minimum and maximum values in the vector v."))

(defmethod vector-minmax ((v vector-t))
  (values (reduce #'min (data v)) (reduce #'max (data v))))

(defgeneric vector-max-index (v)
  (:documentation
   "This function returns the index of the maximum value in the vector
v. When there are several equal maximum elements then the lowest
index is returned."))

(defmethod vector-max-index ((v vector-t))
  (position (reduce #'max (data v)) (data v)))

(defgeneric vector-min-index (v)
  (:documentation
   "This function returns the index of the minimum value in the vector
v. When there are several equal minimum elements then the lowest
index is returned."))

(defmethod vector-min-index ((v vector-t))
  (position (reduce #'min (data v)) (data v)))

(defgeneric vector-minmax-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the vector v. When there are several equal minimum or
maximum elements then the lowest indices are returned."))

(defmethod vector-minmax-index ((v vector-t))
  (values (position (reduce #'min (data v)) (data v))
          (position (reduce #'max (data v)) (data v))))

;; numeric element-type only
(defgeneric vector-isnull (v)
  (:documentation
   "This function return T if all the elements of the vector v are
zero, and NIL otherwise."))

(defmacro make-vector-isnull (element-type)
  (let ((vtype (vector-type element-type))
        (zero (vector-element-nil element-type)))
    `(defmethod vector-isnull ((v ,vtype))
       (dotimes (i (size v) t)
         (if (not (= (aref (data v) (* (stride v) i)) ,zero))
             (return nil))))))

(make-vector-isnull :double)

(make-vector-isnull :float)

(make-vector-isnull :int)

(make-vector-isnull :uint)

;; numeric element-type only
(defgeneric vector-ispos (v)
  (:documentation
   "This function return T if all the elements of the vector v are
strictly positive, and NIL otherwise."))

(defmacro make-vector-ispos (element-type)
  (let ((vtype (vector-type element-type))
        (zero (vector-element-nil element-type)))
    `(defmethod vector-ispos ((v ,vtype))
       (dotimes (i (size v) t)
         (if (<= (aref (data v) (* (stride v) i)) ,zero)
             (return nil))))))

(make-vector-ispos :double)

(make-vector-ispos :float)

(make-vector-ispos :int)

(make-vector-ispos :uint)

;; numeric variable type only
(defgeneric vector-isneg (v)
  (:documentation
   "This function return T if all the elements of the vector v are
strictly negative, and NIL otherwise."))

(defmacro make-vector-isneg (element-type)
  (let ((vtype (vector-type element-type))
        (zero (vector-element-nil element-type)))
    `(defmethod vector-isneg ((v ,vtype))
       (dotimes (i (size v) t)
         (if (>= (aref (data v) (* (stride v) i)) ,zero)
             (return nil))))))

(make-vector-isneg :double)

(make-vector-isneg :float)

(make-vector-isneg :int)

(make-vector-isneg :uint)

;; numeric variable type only
(defgeneric vector-isnonneg (v)
  (:documentation
   "This function return T if all the elements of the vector v are
non-negative respectively, and NIL otherwise."))

(defmacro make-vector-isnonneg (element-type)
  (let ((vtype (vector-type element-type))
        (zero (vector-element-nil element-type)))
    `(defmethod vector-isnonneg ((v ,vtype))
       (dotimes (i (size v) t)
         (if (< (aref (data v) (* (stride v) i)) ,zero)
             (return nil))))))

(make-vector-isnonneg :double)

(make-vector-isnonneg :float)

(make-vector-isnonneg :int)

(make-vector-isnonneg :uint)

;; numeric variable type only
(defgeneric vector-equal (u v)
  (:documentation
   "This function returns T if the vectors u and v are equal (by
comparison of element values) and NIL otherwise."))

(defmacro make-vector-equal (element-type)
  (let ((vtype (vector-type element-type)))
    `(defmethod vector-equal ((u ,vtype) (v ,vtype))
       (if (not (= (size u) (size v)))
           (error "vectors must have same length")
           (dotimes (i (size v) t)
             (if (not (= (aref (data u) (* (stride u) i))
                         (aref (data v) (* (stride v) i))))
                 (return nil)))))))

(make-vector-equal :double)

(make-vector-equal :float)

(make-vector-equal :int)

(make-vector-equal :uint)

(defgeneric vector-read (a &optional str n)
  (:documentation
   "This function reads into the array a from the open stream str. The
array a must be preallocated with the correct length since the
function uses the size of a to determine how many values to read."))

(defmethod vector-read ((a vector-t)
                        &optional (stream *standard-input*) (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (vector-set a i (read stream))))

(defgeneric vector-write (v &optional str n)
  (:documentation
   "This function writes the elements of the array a line-by-line to
the stream str."))

(defmacro make-vector-write (element-type)
  (let ((vtype (vector-type element-type)))
    `(defmethod vector-write ((v ,vtype)
                              &optional (stream *standard-output*) (n nil))
       (let ((s (if (null n) (size v) n)))
         (dotimes (i s v)
           (format stream "~S~%" (vector-get v i)))))))

(make-vector-write :t)

(make-vector-write :double)

(make-vector-write :float)

(make-vector-write :int)

(make-vector-write :uint)

(defvar *print-object-vector-size* 10)

(defun print-vector (v stream)
  (format stream "; ~A vector~%" (size v))
  (if (<= (size v) *print-object-vector-size*)
      (vector-write v stream)
      (progn
        (vector-write v stream *print-object-vector-size*)
        (format stream "; omitted ~A entries~%" (- (size v) *print-object-vector-size*)))))

;; vector-t print-object
(defmethod print-object ((v vector-t) stream)
  (print-vector v stream)
  (call-next-method))

;; vector-t-view print-object
(defmethod print-object ((view vector-t-view) stream)
  (print-vector (shared-vector view) stream)
  (call-next-method))

(defgeneric vector-map (func a &optional n)
  (:documentation
   "Apply function to successive elements of array."))

(defmethod vector-map (func (a vector-t) &optional (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (vector-set a i (funcall func (vector-get a i)))))

(defgeneric vector-reduce (func init a &optional n)
  (:documentation
   "Combine the elements of array using function."))

(defmethod vector-reduce (func init (a vector-t) &optional (n nil))
  (let ((acc init))
    (dotimes (i (if (null n) (size a) n) acc)
      (setf acc (funcall func acc (vector-get a i))))))

(defgeneric vector-count-if (item v &key test)
  (:documentation
   "Return the number of elements in a vector satisfying a test with
item, which defaults to eql."))

(defmethod vector-count-if (item (v vector-t) &key (test #'eql))
  (let ((count 0))
    (dotimes (i (size v) count)
      (if (funcall test item (aref (data v) (* (stride v) i)))
          (incf count)))))

(defgeneric vector-remove-if (item v &key test)
  (:documentation
   "Return a copy of vector with elements satisfying the test (default
is eql) with item removed."))

(defmethod vector-remove-if (item (v vector-t) &key (test #'eql))
  (let ((alt (make-vector (- (size v) (vector-count-if item v :test test))))
        (j 0))
    (dotimes (i (size v) alt)
      (if (not (funcall test item (aref (data v) (* (stride v) i))))
          (progn
            (setf (aref (data alt) (* (stride alt) j))
                  (aref (data v) (* (stride v) i)))
            (incf j))))))
