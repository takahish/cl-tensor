;;;; gsl/histogram-type.lisp
;;;;
;;;; Histograms provide a convenient way of summarizing the
;;;; distribution of a set of data. A histogram consists of a set of
;;;; bins which count the number of events falling into a given range
;;;; of a continuous variable x.  Once a histogram has veen created it
;;;; can also be converted into a probability distribution
;;;; function. The library provides efficient routines for selecting
;;;; random samples from probability distributions. This can be useful
;;;; for generating simulations based on real data.

;;;; Copyright (C) 2017 Takahiro Ishikawa
;;;;
;;;; This program is free software: you can redistribute it and/or modify
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

(in-package "GSL")


;;; gsl_histogram

(defclass histogram ()
  ((pointer :accessor pointer :initarg :pointer)
   (bins :accessor bins :initarg :bins)))


;;; gsl_histogram_pdf

(defclass histogram-pdf ()
  ((pointer :accessor pointer :initarg :pointer)))


;;; histogram allocation

(defun histogram-alloc (number-of-bins)
  "This function allocates memory for a histogram with n bins and
returns a pointer to a newly created histogram. The bins and ranges
are not initialized, and should be prepared using one of the range
setting functions below in order to make the histogram ready for use."
  (let* ((n (coerce number-of-bins
                    `(unsigned-byte
                      ,(* (cffi:foreign-type-size :unsigned-long) 8))))
         (h (make-instance 'histogram
                           :pointer (gsl_histogram_alloc n)
                           :bins n)))
    h))

(defun histogram-set-ranges (h range-list)
  "This function sets the ranges of the existing histogram h using the
range list. The values of the histogram bins are reset to zero. The
range list should contain the desired bin limits. The ranges can be
arbitrary, subject to the restriction that they are monotonically
increasing.
The size of the range list should be defined to be one element bigger
than the number of bins. The additional element is required for the
upper value of the final bin."
  (if (not (= (1+ (bins h)) (length range-list)))
      (error "size of range must match size of histogram")
      (let ((range-array (make-array (1+ (bins h))
                                     :element-type 'double-float)))
        (dotimes (i (1+ (bins h)))
          (setf (aref range-array i)
                (coerce (nth i range-list) 'double-float)))
        (cffi:with-pointer-to-vector-data (range-pointer range-array)
          (gsl_histogram_set_ranges (pointer h) range-pointer (1+ (bins h))))
        h)))

(defun histogram-set-ranges-uniform (h xmin xmax)
  "This function sets the ranges of the existing histogram h to cover
the range xmin to xmax uniformly. The values of the histogram bins are
reset to zero."
  (gsl_histogram_set_ranges_uniform (pointer h)
                                    (coerce xmin 'double-float)
                                    (coerce xmax 'double-float))
  h)

(defun histogram-free (h &optional (result nil))
  "This function frees the histogram h and all of the memory
associated with it."
  (gsl_histogram_free (pointer h))
  result)

(defmacro with-histogram ((var &rest histogram-alloc-args) &body body)
  `(let ((,var (histogram-alloc ,@histogram-alloc-args)))
     (unwind-protect (progn ,@body)
       (histogram-free ,var))))


;;; the histogram probability distribution struct

(defun histogram-pdf-alloc (number-of-bins)
  "This function allocates memory for a probability distribution with
n bins and returns a pointer to a newly initialized histgram-pdf."
  (let* ((n (coerce number-of-bins
                    `(unsigned-byte
                      ,(* (cffi:foreign-type-size :unsigned-long) 8))))
         (p (make-instance 'histogram
                           :pointer (gsl_histogram_pdf_alloc n))))
    p))

(defun histogram-pdf-init (p h)
  "This function initializes the probability distribution p with the
contents of the histogram h."
  (gsi_histogram_pdf_init (pointer p) (pointer h)))

(defun histogram-pdf-free (p)
  "This function frees the probability distribution function p and all
of the memory associated with it."
  (gsl_histogram_pdf_free (pointer p)))

(defmacro with-histogram-pdf ((var &rest histogram-pdf-alloc-args) &body body)
  `(let ((,var (histogram-pdf-alloc ,@histogram-pdf-alloc-args)))
     (unwind-protect (progn ,@body)
       (histogram-pdf-free ,var))))
