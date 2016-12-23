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

(defclass simple-vector-t ()
  ((data :accessor data :initarg :data)
   (size :accessor size :initarg :size)
   (stride :accessor stride :initarg :stride)))

(defclass simple-vector-double (simple-vector-t) ())

(defclass simple-vector-float (simple-vector-t) ())

(defclass simple-vector-int (simple-vector-t) ())

(defclass simple-vector-uint (simple-vector-t) ())

(defun make-simple-vector (n &key (element-type :double)
                              (initial-element nil)
                              (initial-contents nil))
  (cond
    ;; element type: double
    ((and (eql element-type :double)
          (not (null initial-element)))
     (make-instance 'simple-vector-double
                    :data (make-array n
                                      :element-type 'double-float
                                      :initial-element initial-element)
                    :size n
                    :stride 1))
    ((and (eql element-type :double)
          (not (null initial-contents)))
     (make-instance 'simple-vector-double
                    :data (make-array n
                                      :element-type 'double-float
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1))
    ((eql element-type :double)
     (make-instance 'simple-vector-double
                    :data (make-array n
                                      :element-type 'double-float
                                      :initial-element 0.0d0)
                    :size n
                    :stride 1))
    ;; element type: float
    ((and (eql element-type :float)
          (not (null initial-element)))
     (make-instance 'simple-vector-float
                    :data (make-array n
                                      :element-type 'single-float
                                      :initial-element initial-element)
                    :size n
                    :stride 1))
    ((and (eql element-type :float)
          (not (null initial-contents)))
     (make-instance  'simple-vector-float
                     :data (make-array n
                                       :element-type 'single-float
                                       :initial-contents initial-contents)
                     :size n
                     :stride 1))
    ((eql element-type :float)
     (make-instance  'simple-vector-float
                     :data (make-array n
                                       :element-type 'single-float
                                       :initial-element 0.0)
                     :size n
                     :stride 1))
    ;; element type: int
    ((and (eql element-type :int)
          (not (null initial-element)))
     (make-instance 'simple-vector-int
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size n
                    :stride 1))
    ((and (eql element-type :int)
          (not (null initial-contents)))
     (make-instance 'simple-vector-int
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1))
    ((eql element-type :int)
     (make-instance 'simple-vector-int
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size n
                    :stride 1))
    ;; element type: unsigned-int
    ((and (eql element-type :unsigned-int)
          (not (null initial-element)))
     (make-instance 'simple-vector-uint
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size n
                    :stride 1))
    ((and (eql element-type :unsigned-int)
          (not (null initial-contents)))
     (make-instance 'simple-vector-uint
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size n
                    :stride 1))
    ((eql element-type :unsigned-int)
     (make-instance 'simple-vector-uint
                    :data (make-array n
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size n
                    :stride 1))
    (t (error "unsapported element type"))))

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
     (setf (aref (data v) i) ,one)
     v))

(make-simple-vector-set-basis simple-vector-double 0.0d0 1.0d0)

(make-simple-vector-set-basis simple-vector-float 0.0 1.0)

(make-simple-vector-set-basis simple-vector-int 0 1)

(make-simple-vector-set-basis simple-vector-uint 0 1)

(defgeneric simple-vector-subvector (v offset n)
  (:documentation
   "This function return a subvector of another vector v. The start of
the new vector is offset by offset elements from the start of the
original vector."))

(defmacro make-simple-vector-subvector (class element-type)
  `(defmethod simple-vector-subvector ((v ,class) offset n)
     (if (= n 0)
         (error "vector length n must be positive integer")
         (let ((sub (make-simple-vector n :element-type ,element-type)))
           ;; aref delegate range check.
           (dotimes (i n sub)
             (setf (aref (data sub) i)
                   (aref (data v) (* (+ offset i) (stride v)))))))))

(make-simple-vector-subvector simple-vector-double :double)

(make-simple-vector-subvector simple-vector-float :float)

(make-simple-vector-subvector simple-vector-int :int)

(make-simple-vector-subvector simple-vector-uint :unsigned-int)

(defgeneric simple-vector-copy (dest src)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest. The two vectors must have the same length."))

(defmacro make-simple-vector-copy (class)
  `(defmethod simple-vector-copy ((dest ,class) (src ,class))
     (if (not (= (size dest) (size src)))
         (error "vector lengths are not equal")
         (dotimes (j (size src) dest)
           (setf (aref (data dest) (* (stride dest) j))
                 (aref (data src) (* (stride src) j)))))))

(make-simple-vector-copy simple-vector-double)

(make-simple-vector-copy simple-vector-float)

(make-simple-vector-copy simple-vector-int)

(make-simple-vector-copy simple-vector-uint)

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

(defmacro make-simple-vector-add (class)
  `(defmethod simple-vector-add ((a ,class) (b ,class))
     (if (not (= (size a) (size b)))
         (error "vectors must have same length")
         (dotimes (i (size a) a)
           (setf (aref (data a) (* i (stride a)))
                 (+ (aref (data a) (* i (stride a)))
                    (aref (data b) (* i (stride b)))))))))

(make-simple-vector-add simple-vector-double)

(make-simple-vector-add simple-vector-float)

(make-simple-vector-add simple-vector-int)

(make-simple-vector-add simple-vector-uint)

(defgeneric simple-vector-sub (a b)
  (:documentation
   "This function subtracts the elements of vector b from the elements
of vector a. The result ai <- ai − bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-simple-vector-sub (class)
  `(defmethod simple-vector-sub ((a ,class) (b ,class))
     (if (not (= (size a) (size b)))
         (error "vectors must have same length")
         (dotimes (i (size a) a)
           (setf (aref (data a) (* i (stride a)))
                 (- (aref (data a) (* i (stride a)))
                    (aref (data b) (* i (stride b)))))))))

(make-simple-vector-sub simple-vector-double)

(make-simple-vector-sub simple-vector-float)

(make-simple-vector-sub simple-vector-int)

(make-simple-vector-sub simple-vector-uint)

(defgeneric simple-vector-mul (a b)
  (:documentation
   "This function multiplies the elements of vector a by the elements
of vector b. The result ai <- ai ∗ bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-simple-vector-mul (class)
  `(defmethod simple-vector-mul ((a ,class) (b ,class))
     (if (not (= (size a) (size b)))
         (error "vectors must have same length")
         (dotimes (i (size a) a)
           (setf (aref (data a) (* i (stride a)))
                 (* (aref (data a) (* i (stride a)))
                    (aref (data b) (* i (stride b)))))))))

(make-simple-vector-mul simple-vector-double)

(make-simple-vector-mul simple-vector-float)

(make-simple-vector-mul simple-vector-int)

(make-simple-vector-mul simple-vector-uint)

(defgeneric simple-vector-div (a b)
  (:documentation
   "This function divides the elements of vector a by the elements of
vector b. The result ai <- ai/bi is stored in a and b remains
unchanged. The two vectors must have the same length."))

(defmacro make-simple-vector-div (class)
  `(defmethod simple-vector-div ((a ,class) (b ,class))
     (if (not (= (size a) (size b)))
         (error "vectors must have same length")
         (dotimes (i (size a) a)
           (setf (aref (data a) (* i (stride a)))
                 (/ (aref (data a) (* i (stride a)))
                    (aref (data b) (* i (stride b)))))))))

(make-simple-vector-div simple-vector-double)

(make-simple-vector-div simple-vector-float)

(make-simple-vector-div simple-vector-int)

(make-simple-vector-div simple-vector-uint)

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

(defmacro make-simple-vector-isnull (class zero)
  `(defmethod simple-vector-isnull ((v ,class))
     (dotimes (i (size v) t)
       (if (not (= (aref (data v) (* (stride v) i)) ,zero))
           (return nil)))))

(make-simple-vector-isnull simple-vector-double 0.0d0)

(make-simple-vector-isnull simple-vector-float 0.0)

(make-simple-vector-isnull simple-vector-int 0)

(make-simple-vector-isnull simple-vector-uint 0)

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

(defmethod simple-vector-write ((a simple-vector-t)
                                &optional (str *standard-output*) (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (format str "~S~%" (simple-vector-get a i))))

(defmethod simple-vector-write ((a simple-vector-double)
                                &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-VECTOR~%" (if (null n) (size a) n) :double)
  (call-next-method))

(defmethod simple-vector-write ((a simple-vector-float)
                                &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-VECTOR~%" (if (null n) (size a) n) :float)
  (call-next-method))

(defmethod simple-vector-write ((a simple-vector-int)
                                &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-VECTOR~%" (if (null n) (size a) n) :int)
  (call-next-method))

(defmethod simple-vector-write ((a simple-vector-uint)
                                &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-VECTOR~%" (if (null n) (size a) n) :unsigned-int)
  (call-next-method))

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
