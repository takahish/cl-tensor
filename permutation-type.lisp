;;;; permutation-type.lisp

;;;; Copyright (C) 2017 Takahiro Ishikawa
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

(cl:in-package "TENSOR")


;;; element-type

(defvar *permutation-element-type*
  `(unsigned-byte ,(* (cffi:foreign-type-size :int) 8)))


;;; permutation

;; abstract: permutation-t
(defclass permutation-t ()
  ((size :accessor size :initarg :size)
   (data :accessor data :initarg :data)))

(defclass permutation (permutation-t) ())


(defun make-permutation (n)
  (if (<= n 0)
      (error "permutation length n must be positive integer")
      (make-instance 'permutation
                     :size n
                     :data (make-array n
                                       :element-type *permutation-element-type*
                                       :initial-contents (range n)))))


(defgeneric permutation-init (p)
  (:documentation
   "This function initializes the permutation p to the identity,
i.e. (0, 1, 2, . . . , n − 1)."))

(defmethod permutation-init ((p permutation))
  (dotimes (i (size p) p)
    (setf (aref (data p) i) i)))
