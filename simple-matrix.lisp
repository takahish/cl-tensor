;;;; cl-sl/simple-matrix.lisp

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

(cl:in-package "SL")

(defclass simple-matrix-t ()
  ((data :accessor data :initarg :data)
   (size1 :accessor size1 :initarg :size1)
   (size2 :accessor size2 :initarg :size2)
   (tda :accessor tda :initarg :tda)))

(defclass simple-matrix-double (simple-matrix-t) ())

(defclass simple-matrix-float (simple-matrix-t) ())

(defclass simple-matrix-int (simple-matrix-t) ())

(defclass simple-matrix-uint (simple-matrix-t) ())

(defun make-simple-matrix (dim &key (element-type :double)
                                 (initial-element nil)
                                 (initial-contents nil))
  (cond
    ;; element type: double
    ((and (eql element-type :double)
          (not (null initial-element)))
     (make-instance 'simple-matrix-double
                    :data (make-array (apply #'* dim)
                                      :element-type 'double-float
                                      :initial-element initial-element)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((and (eql element-type :double)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-double
                    :data (make-array (apply #'* dim)
                                      :element-type 'double-float
                                      :initial-contents initial-contents)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((eql element-type :double)
     (make-instance 'simple-matrix-double
                    :data (make-array (apply #'* dim)
                                      :element-type 'double-float
                                      :initial-element 0.0d0)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ;; element type: float
    ((and (eql element-type :float)
          (not (null initial-element)))
     (make-instance 'simple-matrix-float
                    :data (make-array (apply #'* dim)
                                      :element-type 'single-float
                                      :initial-element initial-element)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((and (eql element-type :float)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-float
                    :data (make-array (apply #'* dim)
                                      :element-type 'single-float
                                      :initial-element initial-element)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((eql element-type :float)
     (make-instance 'simple-matrix-float
                    :data (make-array (apply #'* dim)
                                      :element-type 'single-float
                                      :initial-element 0.0)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ;; element type: int
    ((and (eql element-type :int)
          (not (null initial-element)))
     (make-instance 'simple-matrix-int
                    :data (make-array (apply #'* dim)
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((and (eql element-type :init)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-int
                    :data (make-array (apply #'* dim)
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((eql element-type :init)
     (make-instance 'simple-matrix-int
                    :data (make-array (apply #'* dim)
                                      ;; imposing limit on the magnitude of an integer
                                      :element-type `(signed-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ;; element type: unsigned-int
    ((and (eql element-type :unsigned-int)
          (not (null initial-element)))
     (make-instance 'simple-matrix-uint
                    :data (make-array (apply #'* dim)
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element initial-element)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((and (eql element-type :unsigned-int)
          (not (null initial-contents)))
     (make-instance 'simple-matrix-uint
                    :data (make-array (apply #'* dim)
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-contents initial-contents)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    ((eql element-type :unsigned-int)
     (make-instance 'simple-matrix-uint
                    :data (make-array (apply #'* dim)
                                      ;; imposing limit on the magnitude of an unsigned integr
                                      :element-type `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8))
                                      :initial-element 0)
                    :size1 (car dim)
                    :size2 (last1 dim)
                    :tda (last1 dim)))
    (t (error "unsapported element type"))))

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
