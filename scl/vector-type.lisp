;;;; scl/vector-type.lisp

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


;;; element-type

(defvar *vector-element-type*
  `((:any . t)
    (:double . double-float)
    (:float . single-float)
    (:int . (signed-byte ,(* (cffi:foreign-type-size :int) 8)))
    (:uint . (unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))))

(defun vector-element-type (element-type)
  (cdr (assoc element-type *vector-element-type*)))

(defvar *vector-element-nil*
  '((:any . nil)
    (:double . 0.0d0)
    (:float . 0.0)
    (:int . 0)
    (:uint . 0)))

(defun vector-element-nil (element-type)
  (cdr (assoc element-type *vector-element-nil*)))

(defvar *vector-element-t*
  '((:any . t)
    (:double . 1.0d0)
    (:float . 1.0)
    (:int . 1)
    (:uint . 1)))

(defun vector-element-t (element-type)
  (cdr (assoc element-type *vector-element-t*)))


;;; data type

(defun vector-data-type (element-type n)
  (cond
    ((eq element-type :any)
     `(simple-vector ,n))
    ((or (eq element-type :double)
         (eq element-type :float)
         (eq element-type :int)
         (eq element-type :uint))
     `(simple-array ,(vector-element-type element-type) (,n)))
    (t nil)))


;;; vector

;; abstract: vector-t
(defclass vector-t ()
  ((data :accessor data :initarg :data)
   (size :accessor size :initarg :size)
   (stride :accessor stride :initarg :stride)
   (owner :accessor owner :initarg :owner)))

(defclass vector-any (vector-t) ())

(defclass vector-double (vector-any) ())

(defclass vector-float (vector-any) ())

(defclass vector-int (vector-any) ())

(defclass vector-uint (vector-any) ())

(defvar *vector-type*
  '((:any . vector-any)
    (:double . vector-double)
    (:float . vector-float)
    (:int . vector-int)
    (:uint . vector-uint)))

(defun vector-type (element-type)
  (cdr (assoc element-type *vector-type*)))

(defun make-vector (n &key (element-type :any)
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

(defclass vector-any-view (vector-t-view) ())

(defclass vector-double-view (vector-any-view) ())

(defclass vector-float-view (vector-any-view) ())

(defclass vector-int-view (vector-any-view) ())

(defclass vector-uint-view (vector-any-view) ())

(defvar *vector-view-type*
  '((:any . vector-any-view)
    (:double . vector-double-view)
    (:float . vector-float-view)
    (:int . vector-int-view)
    (:uint . vector-uint-view)))

(defun vector-view-type (element-type)
  (cdr (assoc element-type *vector-view-type*)))
