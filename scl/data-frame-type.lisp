;;;; scl/data-frame-type.lisp

;;;; Copyright (C) 2017 Takahiro Ishikawa
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


;;; data-frame

(defclass data-frame-t ()
  ((data :accessor data :initarg :data)
   (size1 :accessor size1 :initarg :size1)
   (size2 :accessor size2 :initarg :size2)
   (index :accessor index :initarg :index)
   (names :accessor names :initarg :names)))

(defclass data-frame-any (data-frame-t) ())

(defun make-default-names-list (n2)
  (mapcar #'(lambda (x)
              (intern (concatenate 'string "V" (write-to-string x))))
          (range (1+ n2) :min 1)))

(defun make-default-index-list (n1)
  (range (1+ n1) :min 1))

(defun make-data-frame (n1 n2 &key (initial-element nil) (initial-contents nil)
                                (names nil) (index nil))
  (let ((idx (if (null index) (make-default-index-list n1) index))
        (nms (if (null names) (make-default-names-list n2) names)))
    (cond
      ((not (null initial-element))
       (make-instance 'data-frame-any
                      :data (make-matrix n1 n2 :initial-element initial-element)
                      :size1 n1
                      :size2 n2
                      :index (make-array n1 :initial-contents idx)
                      :names (make-array n2 :initial-contents nms)))
      ((not (null initial-contents))
       (make-instance 'data-frame-any
                      :data (make-matrix n1 n2 :initial-contents (flatten initial-contents))
                      :size1 n1
                      :size2 n2
                      :index (make-array n1 :initial-contents idx)
                      :names (make-array n2 :initial-contents nms)))
      (t
       (make-instance 'data-frame-any
                      :data (make-matrix n1 n2)
                      :size1 n1
                      :size2 n2
                      :index (make-array n1 :initial-contents idx)
                      :names (make-array n2 :initial-contents nms))))))
