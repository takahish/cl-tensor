;;;; sbcl-gsl/vectors.lisp
;;;;
;;;; Vectors are defined by a gsl-vector structure which describes a slice of a block.
;;;; Different vectors can be created which point to the same block. A vector slice is
;;;; a set of equally-spaced elements of an area of memory.

;;;; Copyright (C) 2016 Takahiro Ishikawa
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

(cl:defpackage "SB-GSL-VECTOR"
  (:use "CL"
        "SB-ALIEN"
        "SB-C-CALL"
        "SB-SYS"
        "SB-GSL-BLOCK")
  (:export "GSL-VECTOR"
           "GSL-VECTOR-VIEW"
           "GSL-VECTOR-ALLOC"
           "GSL-VECTOR-CALLOC"
           "GSL-VECTOR-FREE"
           "GSL-VECTOR-GET"
           "GSL-VECTOR-SET"
           "GSL-VECTOR-PTR"
           "GSL-VECTOR-SET-ALL"
           "GSL-VECTOR-SET-ZERO"
           "GSL-VECTOR-SET-BASIS"
           "GSL-VECTOR-MEMCPY"
           "GSL-VECTOR-SWAP"
           "GSL-VECTOR-SWAP-ELEMENTS"
           "GSL-VECTOR-REVERSE"
           "GSL-VECTOR-ADD"
           "GSL-VECTOR-SUB"
           "GSL-VECTOR-MUL"
           "GSL-VECTOR-DIV"
           "GSL-VECTOR-SCALE"
           "GSL-VECTOR-ADD-CONSTANT"
           "GSL-VECTOR-MAX"
           "GSL-VECTOR-MIN"
           "GSL-VECTOR-MINMAX"
           "GSL-VECTOR-MAX-INDEX"
           "GSL-VECTOR-MIN-INDEX"
           "GSL-VECTOR-MINMAX-INDEX"
           "GSL-VECTOR-ISNULL"
           "GSL-VECTOR-ISPOS"
           "GSL-VECTOR-ISNEG"
           "GSL-VECTOR-ISNONNEG"
           "GSL-VECTOR-EQUAL"
           "MAKE-VECTOR"
           "VECTOR-SIZE"
           "VECTOR-STRIDE"
           "VECTOR-DATA"
           "VECTOR-BLOCK"
           "VECTOR-OWNER"
           "VECTOR-GET"
           "VECTOR-SET"
           "VECTOR-PTR"
           "VECTOR-SET-ALL"
           "VECTOR-SET-ZERO"
           "VECTOR-SET-BASIS"
           "VECTOR-MEMCPY"
           "VECTOR-SWAP"
           "VECTOR-SWAP-ELEMENTS"
           "VECTOR-REVERSE"
           "VECTOR-ADD"
           "VECTOR-SUB"
           "VECTOR-MUL"
           "VECTOR-DIV"
           "VECTOR-SCALE"
           "VECTOR-ADD-CONSTANT"
           "VECTOR-MAX"
           "VECTOR-MIN"
           "VECTOR-MINMAX"
           "VECTOR-MAX-INDEX"
           "VECTOR-MIN-INDEX"
           "VECTOR-MINMAX-INDEX"
           "VECTOR-ISNULL"
           "VECTOR-ISPOS"
           "VECTOR-ISNEG"
           "VECTOR-ISNONNEG"
           "VECTOR-ARRAY"))

(cl:in-package "SB-GSL-VECTOR")

;;; (struct gsl-vector)
;;;   The gsl-vector structure contains five components, the size, the stride, a
;;;   pointer to the memory where the elements are stored, data, a pointer to the
;;;   block owned by the vector, block, if any, and an ownership flag, owner.
(define-alien-type nil
    (struct gsl-vector
            (size size-t)
            (stride size-t)
            (data (* double))
            (block (* (struct gsl-block)))
            (owner int)))

;;; (gsl-vector-alloc n)
;;;   This function creates a vector of length n, returning a pointer to a newly initialized
;;;   vector struct. A new block is allocated for the elements of the vector, and stored in
;;;   the block component of the vector struct. The block is "owned" by the vector, and
;;;   will be deallocated when the vector is deallocated.
(define-alien-routine gsl-vector-alloc
    (* (struct gsl-vector))
  (n size-t))

;;; (gsl-vector-calloc n)
;;;   This function allocates memory for a vector of length n and initializes all the elements
;;;   of the vector to zero.
(define-alien-routine gsl-vector-calloc
    (* (struct gsl-vector))
  (n size-t))

;;; (gsl-vector-free v)
;;;   This function frees a previously allocated vector v. If the vector was created using
;;;   gsl-vector-alloc then the block underlying the vector will also be deallocated. If
;;;   the vector has been created from another object then the memory is still owned by
;;;   that object and will not be deallocated.
(define-alien-routine gsl-vector-free
    void
  (v (* (struct gsl-vector))))

;;; (gsl-vector-get v i)
;;;   This function retruns the i-th element of a vector v. If i lies outside the allowed range
;;;   of 0 to n - 1 then the error handler is invoked and 0 is returned.
(define-alien-routine gsl-vector-get
    double
  (v (* (struct gsl-vector)))
  (i size-t))

;;; (gsl-vector-set v i x)
;;;   This function sets the value of the i-th element of a vector v to x. If i lies outside
;;;   the allowed range of 0 to n - 1 then the error handler is invoked.
(define-alien-routine gsl-vector-set
    void
  (v (* (struct gsl-vector)))
  (i size-t)
  (x double))

;;; (gsl-vector-ptr v i)
;;;   This function return a pointer to the i-th element of a vector v. If i lies outside
;;;   the allowed range of 0 to n - 1 then the error handler is invoked and a null pointer is
;;;   returned.
(define-alien-routine gsl-vector-ptr
    (* double)
  (v (* (struct gsl-vector)))
  (i size-t))

;;; (gsl-vector-set-all v x)
;;;   This function sets all the elements of the vector v to the value x.
(define-alien-routine gsl-vector-set-all
    void
  (v (* (struct gsl-vector)))
  (x double))

;;; (gsl-vector-set-zero v)
;;;   This function sets all the elements of the vector v to zero.
(define-alien-routine gsl-vector-set-zero
    void
  (v (* (struct gsl-vector))))

;;; (gsl-vector-set-basis v i)
;;;   This function makes a basis vector by setting all the elements of the vector v to zero
;;;   except for the i-th element which is set to one.
(define-alien-routine gsl-vector-set-basis
    int
  (v (* (struct gsl-vector)))
  (i size-t))

;;; (gsl-vector-memcpy dest src)
;;;   This function copies the elements of the vector src into the vector dest. The two
;;;   vectors must have the same length
(define-alien-routine gsl-vector-memcpy
    int
  (dest (* (struct gsl-vector)))
  (src (* (struct gsl-vector))))

;;; (gsl-vector-swap v w)
;;;   This function exhanges the elements of the vectors v and w by copying. The two
;;;   vectors must have the same length.
(define-alien-routine gsl-vector-swap
    int
  (v (* (struct gsl-vector)))
  (w (* (struct gsl-vector))))

;;; (gsl-vector-swap-elements v i j)
;;;   This function exchanges the i-th and j-th elements of the vector v in-place.
(define-alien-routine gsl-vector-swap-elements
    int
  (v (* (struct gsl-vector)))
  (i size-t)
  (j size-t))

;;; (gsl-vector-reverse v)
;;;   This function reverses the order of the elements of the vector v.
(define-alien-routine gsl-vector-reverse
    int
  (v (* (struct gsl-vector))))

;;; (gsl-vector-add a b)
;;;   This function adds the elements of vector b to the elements of vector a. The result
;;;   a_i <- a_i + b_i is stored in a and b remains unchanged. The two vectors must have
;;;   the same length.
(define-alien-routine gsl-vector-add
    int
  (a (* (struct gsl-vector)))
  (b (* (struct gsl-vector))))

;;; (gsl-vector-sub a b)
;;;   This function subtracts the elements of vector b from the elements of vector a. The
;;;   result a_i <- a_i - b_i is stored in a and b remains unchanged. The two vectors must
;;;   have the same length.
(define-alien-routine gsl-vector-sub
    int
  (a (* (struct gsl-vector)))
  (b (* (struct gsl-vector))))

;;; (gsl-vector-mul a b)
;;;   This function multiplies the elements of vector a by the elements of vector b. The
;;;   result a_i <- a_i * b_i is stored in a and b remains unchanged. The two vectors must
;;;   have the same length
(define-alien-routine gsl-vector-mul
    int
  (a (* (struct gsl-vector)))
  (b (* (struct gsl-vector))))

;;; (gsl-vector-div a b)
;;;   This function divides the elements of vector a by the elements of vector b. The result
;;;   a_i <- a_i / b_i is stored in a and b remains unchanged. The two vectors must have the
;;;   same length.
(define-alien-routine gsl-vector-div
    int
  (a (* (struct gsl-vector)))
  (b (* (struct gsl-vector))))

;;; (gsl-vector-scale a x)
;;;   This function multiplies the elements of vector a by the constant factor x. The result
;;;   a_i <- x * a_i is stored in a.
(define-alien-routine gsl-vector-scale
    int
  (a (* (struct gsl-vector)))
  (x double))

;;; (gsl-vector-add-constant a x)
;;;   This function adds the constant value x to the elements of the vector a. The result
;;;   a_i <- a_i + x is stored in a.
(define-alien-routine gsl-vector-add-constant
    int
  (a (* (struct gsl-vector)))
  (x double))

;;; (gsl-vector-max v)
;;;   This function returns the maximum value in the vector v.
(define-alien-routine gsl-vector-max
    double
  (v (* (struct gsl-vector))))

;;; (gsl-vector-min v)
;;;   This function returns the minimum value in the vector v.
(define-alien-routine gsl-vector-min
    double
  (v (* (struct gsl-vector))))

;;; (gsl-vector-minmax v min-out max-out)
;;;   This function returns the minimum and maximum values in the vector v, storting
;;;   them in min-out and max-out.
(define-alien-routine gsl-vector-minmax
    void
  (v (* (struct gsl-vector)))
  (min-out (* double))
  (max-out (* double)))

;;; (gsl-vector-max-index v)
;;;   This function returns the index of the maximum value in the vector v. When there
;;;   are several equal maximum elements then the lowest index is returned.
(define-alien-routine gsl-vector-max-index
    size-t
  (v (* (struct gsl-vector))))

;;; (gsl-vector-min-index v)
;;;   This function returns the index of the minimum value in the vector v. When there
;;;   are several equal minimum elements then the lowest index is returned.
(define-alien-routine gsl-vector-min-index
    size-t
  (v (* (struct gsl-vector))))

;;; (gsl-vector-minmax-index v imin imax)
;;;   This function returns the indices of the minimum and maximum values in the vector v,
;;;   storing them in imin and imax. When there are several equal minimum or maximum
;;;   elements then the lowest indices are returned.
(define-alien-routine gsl-vector-minmax-index
    void
  (v (* (struct gsl-vector)))
  (imin (* size-t))
  (imax (* size-t)))

;;; (gsl-vector-isnull v)
;;; (gsl-vector-ispos v)
;;; (gsl-vector-isneg v)
;;; (gsl-vector-isnonneg v)
;;;   Theses function return 1 if all the elements of the vector v are zero, strictly positive,
;;;   strictly negative, or non-negative respectively, and 0 otherwise.
(define-alien-routine gsl-vector-isnull
    int
  (v (* (struct gsl-vector))))

(define-alien-routine gsl-vector-ispos
    int
  (v (* (struct gsl-vector))))

(define-alien-routine gsl-vector-isneg
    int
  (v (* (struct gsl-vector))))

(define-alien-routine gsl-vector-isnonneg
    int
  (v (* (struct gsl-vector))))

;;; (gsl-vector-equal u v)
;;;   This function returns 1 if the vector u and v are equal (by comparison of element
;;;   values) and 0 otherwise.
(define-alien-routine gsl-vector-equal
    int
  (u (* (struct gsl-vector)))
  (v (* (struct gsl-vector))))

;;; Allocate an alien of (struct gsl-vector) in foreign heap, and return a pointer to it.
(defun make-vector (n &key (initial-element nil) (initial-contents nil))
  "This function makes a gsl-vector of length n, returning a pointer to a newly initialized
vector struct. A new block is allocated for the elements of the vector, and stored in
the block component of the vector struct. The block is owned by the vector, and will be
deallocated when the vector is deallocated.
The memory is allocated using gsl-vector-alloc, so it can be passed to foreign functions
which gsl-vector-free, or released using free-alien."
  (let ((v (make-alien (struct gsl-vector))))
    (setf v (gsl-vector-calloc n))
    (when (not (null initial-element))
        (gsl-vector-set-all v initial-element))
    (when (not (null initial-contents))
        (dotimes (i n)
          (gsl-vector-set v i (coerce (elt initial-contents i) 'double-float))))
    v))

(defun vector-size (v)
  "This function returns the size of a vector v."
  (slot v 'size))

(defun vector-stride (v)
  "This function returns the stride of a vector v."
  (slot v 'stride))

(defun vector-data (v)
  "This function returns the data of a vector v."
  (slot v 'data))

(defun vector-block (v)
  "This function returns the block of a vector v."
  (slot v 'block))

(defun vector-owner (v)
  "This function returns the owner of a vector v."
  (slot v 'owner))

(defun vector-get (v i)
  "This function retruns the i-th element of a vector v."
  (if (or (< i 0) (< (- (vector-size v) 1) i))
      (error "i must lie inside the range of 0 to (- size 1)")
      (gsl-vector-get v i)))

(defun vector-set (v i x)
  "This function sets the value of the i-th element of a vector v to x."
  (if (or (< i 0) (< (- (vector-size v) 1) i))
      (error "i must lie inside the range of 0 to (- size 1)")
      (progn (gsl-vector-set v i (coerce x 'double-float))
             v)))

(defun vector-ptr (v i)
  "This function return a pointer to the i-th element of a vector v."
  (if (or (< i 0) (< (- (vector-size v) 1) i))
      (error "i must lie inside the range of 0 to (- size 1)")
      (gsl-vector-ptr v i)))

(defun vector-set-all (v x)
  "This function sets all the elements of the vector v to the value x."
  (progn (gsl-vector-set-all v (coerce x 'double-float))
         v))

(defun vector-set-zero (v)
  "This function sets all the elements of the vector v to zero."
  (progn (gsl-vector-set-zero v)
         v))

(defun vector-set-basis (v i)
  "This function makes a basis vector by setting all the elements of the vector v to zero
except for the i-th element which is set to one."
  (if (or (< i 0) (< (- (vector-size v) 1) i))
      (error "i must lie inside the range of 0 to (- size 1)")
      (progn (gsl-vector-set-basis v i)
             v)))

(defun vector-memcpy (src)
  "This function copies the elements of the vector src."
  (let ((dest (make-vector (vector-size src))))
    (gsl-vector-memcpy dest src)
    dest))

(defun vector-swap (v w)
  "This function exhanges the elements of the vectors v and w by copying."
  (progn (gsl-vector-swap v w)
         (values v w)))

(defun vector-swap-elements (v i j)
  "This function exchanges the i-th and j-th elements of the vector v in-place."
  (let ((size-1 (- (vector-size v) 1)))
    (cond ((= i j)
           (error "i and j must be different"))
          ((or (< i 0) (< size-1 i) (< j 0) (< size-1 j))
           (error "i and j must lie inside the range of 0 to (- size 1)"))
          (t
           (progn (gsl-vector-swap-elements v i j)
                  v)))))

(defun vector-reverse (v)
  "This function reverses the order of the elements of the vector v."
  (progn (gsl-vector-reverse v)
         v))

(defun vector-add (a b)
  "This function adds the elements of vector b to the elements of vector a."
  (progn (gsl-vector-add a b)
         a))

(defun vector-sub (a b)
  "This function subtracts the elements of vector b from the elements of vector a."
  (progn (gsl-vector-sub a b)
         a))

(defun vector-mul (a b)
  "This function multiplies the elements of vector a by the elements of vector b."
  (progn (gsl-vector-mul a b)
         a))

(defun vector-div (a b)
  "This function divides the elements of vector a by the elements of vector b."
  (progn (gsl-vector-div a b)
         a))

(defun vector-scale (a x)
  "This function multiplies the elements of vector a by the constant factor x."
  (progn (gsl-vector-scale a (coerce x 'double-float))
         a))

(defun vector-add-constant (a x)
  "This function adds the constant value x to the elements of the vector a."
  (progn (gsl-vector-add-constant a (coerce x 'double-float))
         a))

(defun vector-max (v)
  "This function returns the maximum value in the vector v."
  (gsl-vector-max v))

(defun vector-min (v)
  "This function returns the minimum value in the vector v."
  (gsl-vector-min v))

(defun vector-minmax (v)
  "This function returns the minimum and maximum values in the vector v."
  (with-alien ((min-out double)
               (max-out double))
    (progn (gsl-vector-minmax v (addr min-out) (addr max-out))
           (values min-out max-out))))

(defun vector-max-index (v)
  "This function returns the index of the maximum value in the vector v."
  (gsl-vector-max-index v))

(defun vector-min-index (v)
  "This function returns the index of the minimum value in the vector v."
  (gsl-vector-min-index v))

(defun vector-minmax-index (v)
  "This function returns the indices of the minimum and maximum values in the vector v."
  (with-alien ((imin size-t)
               (imax size-t))
    (progn (gsl-vector-minmax-index v (addr imin) (addr imax))
           (values imin imax))))

(defun vector-isnull (v)
  "This function return 1 if all the elements of the vector v are zero, and 0 otherwise."
  (= (gsl-vector-isnull v) 1))

(defun vector-ispos (v)
  "This function return 1 if all the elements of the vector v are strictly positive,
and 0 otherwise."
  (= (gsl-vector-ispos v) 1))

(defun vector-isneg (v)
  "This function return 1 if all the elements of the vector v are strictly negative,
and 0 otherwise."
  (= (gsl-vector-isneg v) 1))

(defun vector-isnonneg (v)
  "This function return 1 if all the elements of the vector v are non-negative, and
0 otherwise."
  (= (gsl-vector-isnonneg v) 1))

(defun vector-array (v)
  (let ((acc (make-array (vector-size v) :element-type 'double-float)))
    (dotimes (i (vector-size v) acc)
      (setf (aref acc i) (vector-get v i)))))

;;; (test-gsl-vector)
;;;   This function do tests.
(defun test-gsl-vector ()
  (let ((v (make-vector 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9))))
    (dotimes (i (vector-size v) (gsl-vector-free v))
      (format t "P_~A = ~A~%" i (vector-get v i)))))
