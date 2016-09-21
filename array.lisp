;;;; cl-gsl/array.lisp

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

(defclass simple-array-t ()
  ((entity :accessor entity :initarg :entity)
   (size :accessor size :initarg :size)
   (stride :accessor stride :initarg :stride)))

(defclass simple-array-double (simple-array-t) ())

(defclass simple-array-float (simple-array-t) ())

(defclass simple-array-int (simple-array-t) ())

(defclass simple-array-uint (simple-array-t) ())

(defun make-simple-array (n &key (element-type :double)
                              (initial-element nil)
                              (initial-contents nil))
  (cond ((and (eql element-type :double)
              (not (null initial-element)))
         (make-instance 'simple-array-double
                        :entity (make-array n
                                            :element-type 'double-float
                                            :initial-element initial-element)
                        :size n
                        :stride 1))
        ((and (eql element-type :double)
              (not (null initial-contents)))
         (make-instance 'simple-array-double
                        :entity (make-array n
                                            :element-type 'double-float
                                            :initial-contents initial-contents)
                        :size n
                        :stride 1))
        ((eql element-type :double)
         (make-instance 'simple-array-double
                        :entity (make-array n
                                            :element-type 'double-float
                                            :initial-element 0.0d0)
                        :size n
                        :stride 1))
        ((and (eql element-type 'single-float)
              (not (null initial-element)))
         (make-instance 'simple-array-float
                        :entity (make-array n
                                            :element-type 'single-float
                                            :initial-element initial-element)
                        :size n
                        :stride 1))
        ((and (eql element-type :float)
              (not (null initial-contents)))
         (make-instance  'simple-array-float
                         :entity (make-array n
                                             :element-type 'single-float
                                             :initial-contents initial-contents)
                         :size n
                         :stride 1))
        ((eql element-type :float)
         (make-instance  'simple-array-float
                         :entity (make-array n
                                             :element-type 'single-float
                                             :initial-element 0.0)
                         :size n
                         :stride 1))
        ((and (eql element-type :int)
              (not (null initial-element)))
         (make-instance 'simple-array-int
                        :entity (make-array n
                                            ;; imposing limit on the magnitude of an integer
                                            :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                            :initial-element initial-element)
                        :size n
                        :stride 1))
        ((and (eql element-type :int)
              (not (null initial-contents)))
         (make-instance 'simple-array-int
                        :entity (make-array n
                                            ;; imposing limit on the magnitude of an integer
                                            :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                            :initial-contents initial-contents)
                        :size n
                        :stride 1))
        ((eql element-type :int)
         (make-instance 'simple-array-int
                        :entity (make-array n
                                            ;; imposing limit on the magnitude of an integer
                                            :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                            :initial-element 0)
                        :size n
                        :stride 1))
        ((and (eql element-type :unsigned-int)
              (not (null initial-element)))
         (make-instance 'simple-array-uint
                        :entity (make-array n
                                            :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                            :initial-element initial-element)
                        :size n
                        :stride 1))
        ((and (eql element-type :unsigned-int)
              (not (null initial-contents)))
         (make-instance 'simple-array-uint
                        :entity (make-array n
                                            :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                            :initial-contents initial-contents)
                        :size n
                        :stride 1))
        ((eql element-type :unsigned-int)
         (make-instance 'simple-array-uint
                        :entity (make-array n
                                            :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                            :initial-element 0)
                        :size n
                        :stride 1))
        (t (error "unsapported element type"))))

(defgeneric simple-array-get (a i)
  (:documentation
   "This function retruns the i-th element of an array a. If i lies outside the allowed range
of 0 to n - 1 then the error handler is invoked."))

(defmethod simple-array-get ((a simple-array-t) i)
  ;; no range check
  (aref (entity a) (* i (stride a))))

(defgeneric simple-array-set (a i x)
  (:documentation
   "This function sets the value of the i-th element of an array a to x. If i lies outside
the allowed range of 0 to n - 1 then the error handler is invoked."))

(defmethod simple-array-set ((a simple-array-t) i x)
  ;; no range check
  (setf (aref (entity a) (* i (stride a))) x)
  a)

(defgeneric simple-array-read (a &optional str n)
  (:documentation
   "This function reads into the array a from the open stream str. The array a must be
preallocated with the correct length since the function uses the size of a to determine
how many values to read."))

(defmethod simple-array-read ((a simple-array-t) &optional (str *standard-input*) (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (simple-array-set a i (read str))))

(defgeneric simple-array-write (a &optional str n)
  (:documentation
   "This function writes the elements of the array a line-by-line to the stream str."))

(defmethod simple-array-write ((a simple-array-t) &optional (str *standard-output*) (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (format str "~S~%" (simple-array-get a i))))

(defmethod simple-array-write ((a simple-array-double) &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-ARRAY~%" (if (null n) (size a) n) :double)
  (call-next-method))

(defmethod simple-array-write ((a simple-array-float) &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-ARRAY~%" (if (null n) (size a) n) :float)
  (call-next-method))

(defmethod simple-array-write ((a simple-array-int) &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-ARRAY~%" (if (null n) (size a) n) :int)
  (call-next-method))

(defmethod simple-array-write ((a simple-array-uint) &optional (str *standard-output*) (n nil))
  (format str "; ~A ~A SIMPLE-ARRAY~%" (if (null n) (size a) n) :unsigned-int)
  (call-next-method))

(defgeneric simple-array-map (func a &optional n)
  (:documentation
   "Apply function to successive elements of array."))

(defmethod simple-array-map (func (a simple-array-t) &optional (n nil))
  (dotimes (i (if (null n) (size a) n) a)
    (simple-array-set a i (funcall func (simple-array-get a i)))))

(defgeneric simple-array-reduce (func init a &optional n)
  (:documentation
   "Combine the elements of array using function."))

(defmethod simple-array-reduce (func init (a simple-array-t) &optional (n nil))
  (let ((acc init))
    (dotimes (i (if (null n) (size a) n) acc)
      (setf acc (funcall func acc (simple-array-get a i))))))
