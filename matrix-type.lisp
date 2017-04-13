;;;; matrix-type.lisp

;;;; Copyright (C) 2016, 2017 Takahiro Ishikawa
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package "EIGEN")


;;; element-type

(defvar *matrix-element-type*
  `((:any . t)
    (:double . double-float)
    (:float . single-float)
    (:int . (signed-byte ,(* (cffi:foreign-type-size :int) 8)))
    (:uint . (unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))

(defun matrix-element-type (element-type)
  (cdr (assoc element-type *matrix-element-type*)))

(defvar *matrix-element-nil*
  '((:any . nil)
    (:double . 0.0d0)
    (:float . 0.0)
    (:int . 0)
    (:uint . 0)))

(defun matrix-element-nil (element-type)
  (cdr (assoc element-type *matrix-element-nil*)))

(defvar *matrix-element-t*
  '((:any . t)
    (:double . 1.0d0)
    (:float . 1.0)
    (:int . 1)
    (:uint . 1)))

(defun matrix-element-t (element-type)
  (cdr (assoc element-type *vector-element-t*)))


;;; data type

(defun matrix-data-type (element-type n1 n2)
  (cond
    ((eq element-type :any)
     `(simple-vector ,(* n1 n2)))
    ((or (eq element-type :double)
         (eq element-type :float)
         (eq element-type :int)
         (eq element-type :uint))
     `(simple-array ,(vector-element-type element-type) (,(* n1 n2))))
    (t nil)))


;;; matrix

(defclass matrix-t ()
  ((data :accessor data :initarg :data)
   (size1 :accessor size1 :initarg :size1)
   (size2 :accessor size2 :initarg :size2)
   (tda :accessor tda :initarg :tda)
   (owner :accessor owner :initarg :owner)))

(defclass matrix-any (matrix-t) ())

(defclass matrix-double (matrix-any) ())

(defclass matrix-float (matrix-any) ())

(defclass matrix-int (matrix-any) ())

(defclass matrix-uint (matrix-any) ())

(defvar *matrix-type*
  '((:any . matrix-any)
    (:double . matrix-double)
    (:float . matrix-float)
    (:int . matrix-int)
    (:uint . matrix-uint)))

(defun matrix-type (element-type)
  (cdr (assoc element-type *matrix-type*)))

(defun make-matrix (n1 n2 &key (element-type :any)
                            (initial-element nil) (initial-contents nil))
  (cond
    ((not (null initial-element))
     (make-instance (matrix-type element-type)
                    :data (make-array (* n1 n2)
                                      :element-type (matrix-element-type element-type)
                                      :initial-element initial-element)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    ((not (null initial-contents))
     (make-instance (matrix-type element-type)
                    :data (make-array (* n1 n2)
                                      :element-type (matrix-element-type element-type)
                                      :initial-contents initial-contents)
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))
    (t
     (make-instance (matrix-type element-type)
                    :data (make-array (* n1 n2)
                                      :element-type (matrix-element-type element-type)
                                      :initial-element (matrix-element-nil element-type))
                    :size1 n1
                    :size2 n2
                    :tda n2
                    :owner t))))


;;; matrix-view

(defclass matrix-t-view ()
  ((shared-matrix :accessor shared-matrix :initarg :shared-matrix)))

(defclass matrix-any-view (matrix-t-view) ())

(defclass matrix-double-view (matrix-any-view) ())

(defclass matrix-float-view (matrix-any-view) ())

(defclass matrix-int-view (matrix-any-view) ())

(defclass matrix-uint-view (matrix-any-view) ())

(defvar *matrix-view-type*
  '((:any . matrix-any-view)
    (:double . matrix-double-view)
    (:float . matrix-float-view)
    (:int . matrix-int-view)
    (:uint . matrix-uint-view)))

(defun matrix-view-type (element-type)
  (cdr (assoc element-type *matrix-view-type*)))
