;;;; cl-sct/simple-vector.lisp

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

;;; simple-vector

(defclass simple-vector-t ()
  ((data :accessor data :initarg :data)
   (size :accessor size :initarg :size)
   (stride :accessor stride :initarg :stride)
   (owner :accessor owner :initarg :owner)))

(defclass simple-vector-double (simple-vector-t) ())

(defclass simple-vector-float (simple-vector-t) ())

(defclass simple-vector-int (simple-vector-t) ())

(defclass simple-vector-uint (simple-vector-t) ())

(defun make-simple-vector (n &key (element-type :t)
                               (initial-element nil)
                               (initial-contents nil))
  (cond
    ;; default
    ((and (eql element-type :t)
          (not (null initial-element)))
     (make-instance 'simple-vector-t
                    :data (make-array n
                                      :initial-element initial-element)
                    :size n
                    :stride 1
                    :owner t))
    ((and (eql element-type :t)
          (not (null initial-contents)))
     (make-instance 'simple-vector-t
                    :data (make-array n
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :t)
     (make-instance 'simple-vector-t
                    :data (make-array n
                                      :initial-element nil)
                    :size n
                    :stride 1
                    :owner t))
    ;; element type: double
    ((and (eql element-type :double)
          (not (null initial-element)))
     (make-instance 'simple-vector-double
                    :data (make-array n
                                      :element-type 'double-float
                                      :initial-element initial-element)
                    :size n
                    :stride 1
                    :owner t))
    ((and (eql element-type :double)
          (not (null initial-contents)))
     (make-instance 'simple-vector-double
                    :data (make-array n
                                      :element-type 'double-float
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :double)
     (make-instance 'simple-vector-double
                    :data (make-array n
                                      :element-type 'double-float
                                      :initial-element 0.0d0)
                    :size n
                    :stride 1
                    :owner t))
    ;; element type: float
    ((and (eql element-type :float)
          (not (null initial-element)))
     (make-instance 'simple-vector-float
                    :data (make-array n
                                      :element-type 'single-float
                                      :initial-element initial-element)
                    :size n
                    :stride 1
                    :owner t))
    ((and (eql element-type :float)
          (not (null initial-contents)))
     (make-instance  'simple-vector-float
                     :data (make-array n
                                       :element-type 'single-float
                                       :initial-contents initial-contents)
                     :size n
                     :stride 1
                     :owner t))
    ((eql element-type :float)
     (make-instance  'simple-vector-float
                     :data (make-array n
                                       :element-type 'single-float
                                       :initial-element 0.0)
                     :size n
                     :stride 1
                     :owner t))
    ;; element type: int
    ((and (eql element-type :int)
          (not (null initial-element)))
     (make-instance 'simple-vector-int
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size n
                    :stride 1
                    :owner t))
    ((and (eql element-type :int)
          (not (null initial-contents)))
     (make-instance 'simple-vector-int
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :int)
     (make-instance 'simple-vector-int
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size n
                    :stride 1
                    :owner t))
    ;; element type: unsigned-int
    ((and (eql element-type :unsigned-int)
          (not (null initial-element)))
     (make-instance 'simple-vector-uint
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size n
                    :stride 1
                    :owner t))
    ((and (eql element-type :unsigned-int)
          (not (null initial-contents)))
     (make-instance 'simple-vector-uint
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1
                    :owner t))
    ((eql element-type :unsigned-int)
     (make-instance 'simple-vector-uint
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size n
                    :stride 1
                    :owner t))
    (t (error "unsapported element type"))))

;;; simple-vector-view

(defclass simple-vector-t-view ()
  ((shared-vector :accessor shared-vector :initarg :shared-vector)))

(defclass simple-vector-double-view (simple-vector-t-view) ())

(defclass simple-vector-float-view (simple-vector-t-view) ())

(defclass simple-vector-int-view (simple-vector-t-view) ())

(defclass simple-vector-uint-view (simple-vector-t-view) ())

;;; functions

(defgeneric simple-vector-coerce (v element-type))

(defmethod simple-vector-coerce ((v simple-vector-t) element-type)
  (let ((alt (make-simple-vector (size v) :element-type element-type)))
    (dotimes (i (size v) alt)
      (setf (aref (data alt) (* i (stride alt)))
            (aref (data v) (* i (stride v)))))))

(defgeneric simple-vector-get (v i)
  (:documentation
   "This function retruns the i-th element of a vector v. If i lies
outside the allowed range of 0 to n - 1 then the error handler is
invoked."))

(defmethod simple-vector-get ((v simple-vector-t) i)
  ;; aref delegate range check.
  (aref (data v) (* i (stride v))))

(defgeneric simple-vector-set (v i x)
  (:documentation
   "This function sets the value of the i-th element of a vector v to
x. If i lies outside the allowed range of 0 to n - 1 then the error
handler is invoked."))

(defmethod simple-vector-set ((v simple-vector-t) i x)
  ;; aref delegate range check.
  (setf (aref (data v) (* i (stride v))) x)
  v)

(defgeneric simple-vector-set-all (v x)
  (:documentation
   "This function sets all the elements of the vector v to the value x."))

(defmethod simple-vector-set-all ((v simple-vector-t) x)
  (dotimes (i (size v) v)
    (setf (aref (data v) (* i (stride v))) x)))

(defgeneric simple-vector-set-zero (v)
  (:documentation
   "This function sets all the elements of the vector v to zero."))

(defmacro make-simple-vector-set-zero (class zero)
  `(defmethod simple-vector-set-zero ((v ,class))
     (dotimes (i (size v) v)
       (setf (aref (data v) (* i (stride v))) ,zero))))

(make-simple-vector-set-zero simple-vector-t nil)

(make-simple-vector-set-zero simple-vector-double 0.0d0)

(make-simple-vector-set-zero simple-vector-float 0.0)

(make-simple-vector-set-zero simple-vector-int 0)

(make-simple-vector-set-zero simple-vector-uint 0)

(defgeneric simple-vector-set-basis (v i)
  (:documentation
   "This function makes a basis vector by setting all the elements of
the vector v to zero except for the i-th element which is set to one."))

(defmacro make-simple-vector-set-basis (class zero one)
  `(defmethod simple-vector-set-basis ((v ,class) i)
     (dotimes (j (size v))
       (setf (aref (data v) (* j (stride v))) ,zero))
     ;; aref delegate range check.
     (setf (aref (data v) (* i (stride v))) ,one)
     v))

(make-simple-vector-set-basis simple-vector-t nil t)

(make-simple-vector-set-basis simple-vector-double 0.0d0 1.0d0)

(make-simple-vector-set-basis simple-vector-float 0.0 1.0)

(make-simple-vector-set-basis simple-vector-int 0 1)

(make-simple-vector-set-basis simple-vector-uint 0 1)

(defgeneric simple-vector-subvector (v offset n)
  (:documentation
   "This function return a vector view of a subvector of another
vector v. The start of the new vector is offset by offset elements
from the start of the original vector. The new vector has n
elements."))

(defmacro make-simple-vector-subvector (vector-class view-class)
  `(defmethod simple-vector-subvector ((v ,vector-class) offset n)
     (cond
       ((<= n 0)
        (error "vector length n must be positive integer"))
       ((>= (+ offset (- n 1)) (size v))
        (error "view would extend past end of vector"))
       (t
        (let ((view (make-instance (quote ,view-class)))
              (sub (make-instance (quote ,vector-class)))
              (element-type (second (type-of (data v)))))
          ;; set sub
          (setf (data sub) (make-array n
                                       :displaced-to (data v)
                                       :displaced-index-offset offset
                                       :element-type element-type)
                (size sub) n
                (stride sub) (stride v)
                (owner sub) nil)
          ;; set view
          (setf (shared-vector view) sub)
          view)))))

(make-simple-vector-subvector simple-vector-t
                              simple-vector-t-view)

(make-simple-vector-subvector simple-vector-double
                              simple-vector-double-view)

(make-simple-vector-subvector simple-vector-float
                              simple-vector-float-view)

(make-simple-vector-subvector simple-vector-int
                              simple-vector-int-view)

(make-simple-vector-subvector simple-vector-uint
                              simple-vector-uint-view)

(defun simple-vector-view-array (base n &key (element-type :t))
  "This function return a vector view of an array. The start of the
new vector is given by base and has n elements."
  (if (= n 0)
      (error "vector length n must be positive integer")
      (cond
        ;; default
        ((and (eql element-type :t)
              (typep base `(simple-vector ,n)))
         (let ((view (make-instance 'simple-vector-t-view))
               (v (make-instance 'simple-vector-t)))
           (setf (data v) base
                 (size v) n
                 (stride v) 1
                 (owner v) nil)
           (setf (shared-vector view) v)
           view))
        ;; element type: double
        ((and (eql element-type :double)
              (typep base `(simple-array double-float (,n))))
         (let ((view (make-instance 'simple-vector-double-view))
               (v (make-instance 'simple-vector-double)))
           (setf (data v) base
                 (size v) n
                 (stride v) 1
                 (owner v) nil)
           (setf (shared-vector view) v)
           view))
        ;; element type: float
        ((and (eql element-type :float)
              (typep base `(simple-array single-float (,n))))
         (let ((view (make-instance 'simple-vector-float-view))
               (v (make-instance 'simple-vector-float)))
           (setf (data v) base
                 (size v) n
                 (stride v) 1
                 (owner v) nil)
           (setf (shared-vector view) v)
           view))
        ;; element type: int
        ((and (eql element-type :int)
              (typep base `(simple-array (signed-byte ,(* (cffi:foreign-type-size :int) 8)) (,n))))
         (let ((view (make-instance 'simple-vector-int-view))
               (v (make-instance 'simple-vector-int)))
           (setf (data v) base
                 (size v) n
                 (stride v) 1
                 (owner v) nil)
           (setf (shared-vector view) v)
           view))
        ;; element type: unsigned-int
        ((and (eql element-type :unsigned-int)
              (typep base `(simple-array (unsigned-byte ,(* (cffi:foreign-type-size :int) 8)) (,n))))
          (let ((view (make-instance 'simple-vector-uint-view))
                (v (make-instance 'simple-vector-uint)))
            (setf (data v) base
                  (size v) n
                  (stride v) 1
                  (owner v) nil)
            (setf (shared-vector view) v)
            view))
        (t "unsapported element type"))))

(defgeneric simple-vector-copy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest. The two vectors must have the same length."))

(defmethod simple-vector-copy ((dest simple-vector-t)
                               (src simple-vector-t))
  (if (not (= (size dest) (size src)))
      (error "vector lengths are not equal")
      (dotimes (j (size src) dest)
        (setf (aref (data dest) (* (stride dest) j))
              (aref (data src) (* (stride src) j))))))

(defgeneric simple-vector-swap-elements (v i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the vector v
in-place."))

(defmethod simple-vector-swap-elements ((v simple-vector-t) i j)
  ;; aref delegate range check.
  (if (not (= i j))
      (progn
        (rotatef (aref (data v) i) (aref (data v) j))
        v)
      v))

(defgeneric simple-vector-reverse (v)
  (:documentation
   "This function reverses the order of the elements of the vector v."))

(defmethod simple-vector-reverse ((v simple-vector-t))
  ;; identity wrap up nreaverse, because STYLE-WARNING has occurred.
  (identity (nreverse (data v)))
  v)

(defgeneric simple-vector-add (a b)
  (:documentation
   "This function adds the elements of vector b to the elements of
vector a. The result ai <- ai + bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod simple-vector-add ((a simple-vector-t) (b simple-vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (+ (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric simple-vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements
of vector a. The result ai <- ai − bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod simple-vector-sub ((a simple-vector-t) (b simple-vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (- (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric simple-vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements
of vector b. The result ai <- ai ∗ bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod simple-vector-mul ((a simple-vector-t) (b simple-vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (* (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric simple-vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of
vector b. The result ai <- ai/bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmethod simple-vector-div ((a simple-vector-t) (b simple-vector-t))
  (if (not (= (size a) (size b)))
      (error "vectors must have same length")
      (dotimes (i (size a) a)
        (setf (aref (data a) (* i (stride a)))
              (/ (aref (data a) (* i (stride a)))
                 (aref (data b) (* i (stride b))))))))

(defgeneric simple-vector-scale (a x)
  (:documentation
   "This function multiplies the elements of vector a by the constant
factor x. The result ai <- xai is stored in a."))

(defmethod simple-vector-scale ((a simple-vector-t) x)
  (dotimes (i (size a) a)
    (setf (aref (data a) (* i (stride a)))
          (* (aref (data a) (* i (stride a))) x))))

(defgeneric simple-vector-add-constant (a x)
  (:documentation
   "This function adds the constant value x to the elements of the
vector a. The result ai <- ai + x is stored in a."))

(defmethod simple-vector-add-constant ((a simple-vector-t) x)
  (dotimes (i (size a) a)
    (setf (aref (data a) (* i (stride a)))
          (+ (aref (data a) (* i (stride a))) x))))

(defgeneric simple-vector-max (v)
  (:documentation
   "This function returns the maximum value in the vector v."))

(defmethod simple-vector-max ((v simple-vector-t))
  (reduce #'max (data v)))

(defgeneric simple-vector-min (v)
  (:documentation
   "This function returns the minimum value in the vector v."))

(defmethod simple-vector-min ((v simple-vector-t))
  (reduce #'min (data v)))

(defgeneric simple-vector-minmax (v)
  (:documentation
   "This function returns the minimum and maximum values in the vector v."))

(defmethod simple-vector-minmax ((v simple-vector-t))
  (values (reduce #'min (data v)) (reduce #'max (data v))))

(defgeneric simple-vector-max-index (v)
  (:documentation
   "This function returns the index of the maximum value in the vector
v. When there are several equal maximum elements then the lowest
index is returned."))

(defmethod simple-vector-max-index ((v simple-vector-t))
  (position (reduce #'max (data v)) (data v)))

(defgeneric simple-vector-min-index (v)
  (:documentation
   "This function returns the index of the minimum value in the vector
v. When there are several equal minimum elements then the lowest
index is returned."))

(defmethod simple-vector-min-index ((v simple-vector-t))
  (position (reduce #'min (data v)) (data v)))

(defgeneric simple-vector-minmax-index (v)
  (:documentation
   "This function returns the indices of the minimum and maximum
values in the vector v. When there are several equal minimum or
maximum elements then the lowest indices are returned."))

(defmethod simple-vector-minmax-index ((v simple-vector-t))
  (values (position (reduce #'min (data v)) (data v))
          (position (reduce #'max (data v)) (data v))))

(defgeneric simple-vector-isnull (v)
  (:documentation
   "This function return T if all the elements of the vector v are
zero, and NIL otherwise."))

(defmethod simple-vector-isnull ((v simple-vector-t))
  (dotimes (i (size v) t)
    (if (not (null (aref (data v) (* (stride v) i))))
        (return nil))))

(defmacro make-simple-vector-isnull (class zero)
  `(defmethod simple-vector-isnull ((v ,class))
     (dotimes (i (size v) t)
       (if (not (= (aref (data v) (* (stride v) i)) ,zero))
           (return nil)))))

(make-simple-vector-isnull simple-vector-double 0.0d0)

(make-simple-vector-isnull simple-vector-float 0.0)

(make-simple-vector-isnull simple-vector-int 0)

(make-simple-vector-isnull simple-vector-uint 0)

;; numeric variable type only
(defgeneric simple-vector-ispos (v)
  (:documentation
   "This function return T if all the elements of the vector v are
strictly positive, and NIL otherwise."))

(defmacro make-simple-vector-ispos (class zero)
  `(defmethod simple-vector-ispos ((v ,class))
     (dotimes (i (size v) t)
       (if (<= (aref (data v) (* (stride v) i)) ,zero)
           (return nil)))))

(make-simple-vector-ispos simple-vector-double 0.0d0)

(make-simple-vector-ispos simple-vector-float 0.0)

(make-simple-vector-ispos simple-vector-int 0)

(make-simple-vector-ispos simple-vector-uint 0)

;; numeric variable type only
(defgeneric simple-vector-isneg (v)
  (:documentation
   "This function return T if all the elements of the vector v are
strictly negative, and NIL otherwise."))

(defmacro make-simple-vector-isneg (class zero)
  `(defmethod simple-vector-isneg ((v ,class))
     (dotimes (i (size v) t)
       (if (>= (aref (data v) (* (stride v) i)) ,zero)
           (return nil)))))

(make-simple-vector-isneg simple-vector-double 0.0d0)

(make-simple-vector-isneg simple-vector-float 0.0)

(make-simple-vector-isneg simple-vector-int 0)

(make-simple-vector-isneg simple-vector-uint 0)

;; numeric variable type only
(defgeneric simple-vector-isnonneg (v)
  (:documentation
   "This function return T if all the elements of the vector v are
non-negative respectively, and NIL otherwise."))

(defmacro make-simple-vector-isnonneg (class zero)
  `(defmethod simple-vector-isnonneg ((v ,class))
     (dotimes (i (size v) t)
       (if (< (aref (data v) (* (stride v) i)) ,zero)
           (return nil)))))

(make-simple-vector-isnonneg simple-vector-double 0.0d0)

(make-simple-vector-isnonneg simple-vector-float 0.0)

(make-simple-vector-isnonneg simple-vector-int 0)

(make-simple-vector-isnonneg simple-vector-uint 0)

;; numeric variable type only
(defgeneric simple-vector-equal (u v)
  (:documentation
   "This function returns T if the vectors u and v are equal (by
comparison of element values) and NIL otherwise."))

(defmacro make-simple-vector-equal (class)
  `(defmethod simple-vector-equal ((u ,class) (v ,class))
    (if (not (= (size u) (size v)))
        (error "vectors must have same length")
        (dotimes (i (size v) t)
          (if (not (= (aref (data u) (* (stride u) i))
                      (aref (data v) (* (stride v) i))))
              (return nil))))))

(make-simple-vector-equal simple-vector-double)

(make-simple-vector-equal simple-vector-float)

(make-simple-vector-equal simple-vector-int)

(make-simple-vector-equal simple-vector-uint)

(defgeneric simple-vector-read (a &optional str n)
  (:documentation
   "This function reads into the array a from the open stream str. The
array a must be preallocated with the correct length since the
function uses the size of a to determine how many values to read."))

(defmethod simple-vector-read ((a simple-vector-t)
                               &optional (str *standard-input*) (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (simple-vector-set a i (read str))))

(defgeneric simple-vector-write (a &optional str n)
  (:documentation
   "This function writes the elements of the array a line-by-line to
the stream str."))

(defmacro make-simple-vector-write (class element-type)
  `(defmethod simple-vector-write ((a ,class)
                                   &optional (str *standard-output*) (n nil))
     (let ((s (if (null n) (size a) n)))
       (format str "; ~A ~A SIMPLE-VECTOR~%" s ,element-type)
       (dotimes (i s a)
         (format str "~S~%" (simple-vector-get a i))))))

(make-simple-vector-write simple-vector-t :t)

(make-simple-vector-write simple-vector-double :double)

(make-simple-vector-write simple-vector-float :float)

(make-simple-vector-write simple-vector-int :int)

(make-simple-vector-write simple-vector-uint :unsigned-int)

(defgeneric simple-vector-map (func a &optional n)
  (:documentation
   "Apply function to successive elements of array."))

(defmethod simple-vector-map (func (a simple-vector-t) &optional (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (simple-vector-set a i (funcall func (simple-vector-get a i)))))

(defgeneric simple-vector-reduce (func init a &optional n)
  (:documentation
   "Combine the elements of array using function."))

(defmethod simple-vector-reduce (func init (a simple-vector-t) &optional (n nil))
  (let ((acc init))
    (dotimes (i (if (null n) (size a) n) acc)
      (setf acc (funcall func acc (simple-vector-get a i))))))
