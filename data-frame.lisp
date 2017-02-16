;;;; cl-sct/data-frame.lisp

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

(cl:in-package "SCT")


;;; data-frame

(defclass data-frame-t ()
  ((data :accessor data :initarg :data)
   (size1 :accessor size1 :initarg :size1)
   (size2 :accessor size2 :initarg :size2)
   (index :accessor index :initarg :index)
   (names :accessor names :initarg :names)))

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
       (make-instance 'data-frame-t
                      :data (make-matrix n1 n2 :initial-element initial-element)
                      :size1 n1
                      :size2 n2
                      :index (make-array n1 :initial-contents idx)
                      :names (make-array n2 :initial-contents nms)))
      ((not (null initial-contents))
       (make-instance 'data-frame-t
                      :data (make-matrix n1 n2 :initial-contents (flatten initial-contents))
                      :size1 n1
                      :size2 n2
                      :index (make-array n1 :initial-contents idx)
                      :names (make-array n2 :initial-contents nms)))
      (t
       (make-instance 'data-frame-t
                      :data (make-matrix n1 n2)
                      :size1 n1
                      :size2 n2
                      :index (make-array n1 :initial-contents idx)
                      :names (make-array n2 :initial-contents nms))))))


;;; function

(defun data-frame-row (df index)
  "This function returns a vector view of the row of index of the
data-frame df."
  (matrix-row (data df) (position index (index df))))

(defun data-frame-column (df name)
  "This function returns a vector view of the column of name of the
data-frame df."
  (matrix-column (data df) (position name (names df))))

(defun data-frame-get-row (df index)
  "This function returns a vector of the row of index of the
data-frame df."
  (let ((row (make-vector (size2 (data df)))))
    (matrix-get-row row (data df) (position index (index df)))
    row))

(defun data-frame-set-row (df index row)
  "This function opies the elements of the row into the row of index
of the data-frame df."
  (matrix-set-row (data df) (position index (index df)) row)
  df)

(defun data-frame-get-col (df name)
  "This function returns a vector of the column of name of data-frame
df."
  (let ((col (make-vector (size1 (data df)))))
    (matrix-get-col col (data df)  (position name (names df)))
    col))

(defun data-frame-set-col (df name col)
  "This function copies the elements of the column into the column of
name of data-frame df."
  (matrix-set-col (data df) (position name (names df)) col)
  df)

(defun data-frame-read (df &optional (stream *standard-output*))
  "This function reads into the data-frame df from the open stream
stream in binary format. The data-frame df must be preallocated with
the correct dimensions since the function uses the size of df to
determine how many bytes to read."
  (matrix-read (data df) stream))

(defun data-frame-write (df &optional (stream *standard-output*) (n1 nil) (n2 nil))
  "This function writes the elements of the data-frame df to the
stream stream."
  (let ((s1 (if (null n1) (size1 df) n1))
        (s2 (if (null n2) (size2 df) n2)))
    (format stream "~C" #\tab)
    (dotimes (j s2)
      (format stream "~S~C" (aref (names df) j) #\tab))
    (format stream "~%")
    (dotimes (i s1 df)
      (format stream "~S~C" (aref (index df) i) #\tab)
      (dotimes (j s2)
        (if (= j (1- s2))
            (format stream "~S~%" (matrix-get (data df) i j))
            (format stream "~S~C" (matrix-get (data df) i j) #\tab))))))

(defvar *print-object-data-frame-size1* 10)

(defvar *print-object-data-frame-size2* 10)

(defun print-data-frame (df stream)
  (format stream "; ~A x ~A data-frame~%" (size1 df) (size2 df))
  (cond ((and (<= (size1 df) *print-object-data-frame-size1*)
              (<= (size2 df) *print-object-data-frame-size2*))
         (data-frame-write df stream))
        ((and (> (size1 df) *print-object-data-frame-size1*)
              (<= (size2 df) *print-object-data-frame-size2*))
         (data-frame-write df stream *print-object-data-frame-size1* (size2 df))
         (format stream "; omitted ~A rows~%"
                 (- (size1 df) *print-object-data-frame-size1*)))
        ((and (<= (size1 df) *print-object-data-frame-size1*)
              (> (size2 df) *print-object-data-frame-size2*))
         (data-frame-write df stream (size1 df) *print-object-data-frame-size2*)
         (format stream "; omitted ~A columns~%"
                 (- (size2 df) *print-object-data-frame-size2*)))
        ((and (> (size1 df) *print-object-data-frame-size1*)
              (> (size2 df) *print-object-data-frame-size2*))
         (data-frame-write df stream *print-object-data-frame-size1*
                           *print-object-data-frame-size2*)
         (format stream "; omitted ~A rows and ~A columns~%"
                 (- (size1 df) *print-object-data-frame-size1*)
                 (- (size2 df) *print-object-data-frame-size2*)))))

; data-frame-t print-object
(defmethod print-object ((df data-frame-t) stream)
  (print-data-frame df stream)
  (call-next-method))

(defun data-frame-read-tsv (df file)
  (with-open-file (stream file :direction :input)
    (data-frame-read df stream))
  df)
