;;;; permutation.lisp

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


;;; basic functions

(defgeneric permutation-copy (dest src)
  (:documentation
   "This function copies the elements of the permutation src into the
permutation dest. The two permutations must have the same size."))

(defmethod permutation-copy ((dest permutation) (src permutation))
  (if (not (= (size src) (size dest)))
      (error "permutation lengths are not equal")
      (dotimes (j (size src) src)
        (setf (aref (data src) j) (aref (data dest) j)))))


(defgeneric permutation-get (p i)
  (:documentation
   "This function returns the value of the i-th element of the
permutation p. If i lies outside the allowed range of 0 to n − 1 then
the error handler is invoked."))

(defmethod permutation-get ((p permutation) i)
  ;; aref delegate range check.
  (aref (data p) i))


(defgeneric permutation-swap (p i j)
  (:documentation
   "This function exchanges the i-th and j-th elements of the
permutation p."))

(defmethod permutation-swap ((p permutation) i j)
  ;; aref delegate range check.
  (if (not (= i j))
      (progn
        (rotatef (aref (data p) i) (aref (data p) j))
        p)
      p))


(defgeneric permutation-valid (p)
  (:documentation
   "This function checks that the permutation p is valid. The n
elements should contain each of the numbers 0 to n − 1 once and only
once."))

(defmethod permutation-valid ((p permutation))
  (let ((size (size p)))
    (dotimes (i size t)
      (if (>= (aref (data p) i) size)
          (error "permutation index outside range")
          (dotimes (j i)
            (if (= (aref (data p) i) (aref (data p) j))
                (error "duplicate permutation index")))))))


(defgeneric permutation-reverse (p)
  (:documentation
   "This function reverses the elements of the permutation p."))

(defmethod permutation-reverse ((p permutation))
  ;; identity wrap up nreaverse, because STYLE-WARNING has occurred.
  (identity (nreverse (data p)))
  p)


(defgeneric permutation-inverse (inv p)
  (:documentation
   "This function computes the inverse of the permutation p, storing
the result in inv."))

(defmethod permutation-inverse ((inv permutation) (p permutation))
  (let ((size (size p)))
    (if (not (= (size inv) size))
        (error "permutation lengths are not equal")
        (dotimes (i size inv)
          (setf (aref (data inv) (aref (data p) i)) i)))))


(defgeneric permutation-next (p)
  (:documentation
   "This function advances the permutation p to the next permutation
in lexicographic order. If no further permutations are available it
leaves p unmodified. Starting with the identity permutation and
repeatedly applying this function will iterate through all possible
permutations of a given order.
Replaces p with the next permutation (in the standard lexicographical
ordering) and t. Returns current p and nil If there is no next
permutation."))

(defmethod permutation-next ((p permutation))
  (let ((size (size p)) i k)
    (if (< size 2)
        (values p nil) ; no next permutation
        (progn
          (setf i (- size 2))
          (while (and (> (aref (data p) i) (aref (data p) (+ i 1)))
                      (not (= i 0)))
            (setf i (- i 1)))
          (if (and (= i 0) (> (aref (data p) 0) (aref (data p) 1)))
              (values p nil) ; no next permutation
              (progn
                (setf k (+ i 1))
                (do ((j (+ i 2) (+ j 1)))
                    ((>= j size))
                  (if (and (> (aref (data p) j) (aref (data p) i))
                           (< (aref (data p) j) (aref (data p) k)))
                      (setf k j)))
                ;; swap i and k
                (rotatef (aref (data p) i) (aref (data p) k))
                (do ((j (+ i 1) (+ j 1)))
                    ((> j (/ (+ size i) 2)))
                  (rotatef (aref (data p) j) (aref (data p) (- (+ size i) j))))
                (values p t)))))))


(defgeneric permutation-prev (p)
  (:documentation
   "This function steps backwards from the permutation p to the
previous permutation in lexicographic order. If no previous
permutation is available it leaves p unmodified.
Replaces p with the prev permutation (in the standard lexicographical
ordering) and t. Returns current p and nil If there is no prev
permutation."))

(defmethod permutation-prev ((p permutation))
  (let ((size (size p)) i k)
    (if (< size 2)
        (values p nil) ; no prev permutation
        (progn
          (setf i (- size 2))
          (while (and (< (aref (data p) i) (aref (data p) (+ i 1)))
                      (not (= i 0)))
            (setf i (- i 1)))
          (if (and (= i 0) (< (aref (data p) 0) (aref (data p) 1)))
              (values p nil) ; no prev permutation
              (progn
                (setf k (+ i 1))
                (do ((j (+ i 2) (+ j 1)))
                    ((>= j size))
                  (if (and  (< (aref (data p) j) (aref (data p) i))
                            (> (aref (data p) j) (aref (data p) k)))
                      (setf k j)))
                ;; swap i and k
                (rotatef (aref (data p) i) (aref (data p) k))
                (do ((j (+ i 1) (+ j 1)))
                    ((> j (/ (+ size i) 2)))
                  (rotatef (aref (data p) j) (aref (data p) (- (+ size i) j))))
                (values p t)))))))


(defgeneric permutation-mul (p pa pb)
  (:documentation
   "This function combines the two permutations pa and pb into a
single permutation p, where p = pa * pb. The permutation p is
equivalent to applying pb first and then pa."))

(defmethod permutation-mul ((p permutation) (pa permutation) (pb permutation))
  (let ((size (size p)))
    (cond
      ((not (= (size pa) size))
       (error "size of result does not match size of pa"))
      ((not (= (size pb) size))
       (error "size of result does not match size of pb"))
      (t
       (dotimes (i size p)
         (setf (aref (data p) i)
               (aref (data pb) (aref (data pa) i))))))))


;;; permute

(defun permute (p data stride n)
  "This function applies the permutation p to the array data of size n
with stride stride."
  (let (k pk tmp r1)
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (setf k (aref p i))
      (while (> k i)
        (setf k (aref p k)))
      (if (< k i)
          (go end))
      ;; now have k == i, i.e the least in its cycle
      (setf pk (aref p k))
      (if (= pk i)
          (go end))
      ;; shuffle the elements of the cycle
      (setf tmp (aref data (* i stride)))
      (while (not (= pk i))
        (setf r1 (aref data (* pk stride))
              (aref data (* k stride)) r1
              k pk
              pk (aref p k)))
      (setf (aref data (* k stride)) tmp)
     end)
    data))


(defun permute-inverse (p data stride n)
  "This function applies the inverse of the permutation p to the array
data of size n with stride stride."
  (let (k pk tmp r1)
    (do ((i 0 (+ i 1)))
        ((>= i n))
      (setf k (aref p i))
      (while (> k i)
        (setf k (aref p k)))
      (if (< k i)
          (go end))
      ;; now have k == i, i.e the least in its cycle
      (setf pk (aref p k))
      (if (= pk i)
          (go end))
      ;; shuffle the elements of the cycle in the inverse direction
      (setf tmp (aref data (* k stride)))
      (while (not (= pk i))
        (setf r1 (aref data (* pk stride))
              (aref data (* pk stride)) tmp
              tmp r1
              k pk
              pk (aref p k)))
      (setf (aref data (* pk stride)) tmp)
     end)
    data))


(defgeneric permute-vector (p v)
  (:documentation
   "This function applies the permutation p to the elements of the
vector v, considered as a row-vector acted on by a permutation matrix
from the right, v′ = vP. The j-th column of the permutation matrix P
is given by the pj-th column of the identity matrix. The permutation p
and the vector v must have the same length."))

(defmethod permute-vector ((p permutation) (v vector-any))
  (if (not (= (size v) (size p)))
      (error "vector and permutation must be the same length")
      (progn
        (permute (data p) (data v) (stride v) (size v))
        v)))


(defgeneric permute-vector-inverse (p v)
  (:documentation
   "This function applies the inverse of the permutation p to the
elements of the vector v, considered as a row-vector acted on by an
inverse permutation matrix from the right, v′ = vPT. Note that for
permutation matrices the inverse is the same as the transpose. The
j-th column of the permutation matrix P is given by the pj-th column
of the identity matrix. The permutation p and the vector v must have
the same length."))

(defmethod permute-vector-inverse ((p permutation) (v vector-any))
  (if (not (= (size v) (size p)))
      (error "vector and permutation must be the same length")
      (progn
        (permute-inverse (data p) (data v) (stride v) (size v))
        v)))


(defgeneric permute-matrix (p a)
  (:documentation
   "This function applies the permutation p to the matrix A from the
right, A′ = AP . The j-th column of the permutation matrix P is given
by the pj-th column of the identity matrix. This effectively permutes
the columns of A according to the permutation p, and so the number of
columns of A must equal the size of the permutation p."))

(defmethod permute-matrix ((p permutation) (a matrix-any))
  (if (not (= (size2 a) (size p)))
      (error "matrix columns and permutation must be the same length")
      (let (view)
        (dotimes (i (size1 a) a)
          (setf view (matrix-row a i))
          (permute-vector p (shared-vector view))))))


;; permutations in cyclic form

(defgeneric permutation-linear-to-canonical (q p)
  (:documentation
   "This function computes the canonical form of the permutation p and
stores it in the output argument q."))

(defmethod permutation-linear-to-canonical ((q permutation) (p permutation))
  (if (not (= (size q) (size p)))
      (error "size of q does not match size of p")
      (let ((n (size p))
            (pp (data p))
            (qq (data q))
            k s tmp)
        (setf tmp n)
        (block rep
          (do ((i 0 (+ i 1)))
              ((>= i n))
            (setf k (aref pp i)
                  s 1)
            (while (> k i)
              (setf k (aref pp k)
                    s (+ s 1)))
            (if (< k i)
                (go end))
            ;; now have k == i. i.e the least in its cycle, and s == cycle length
            (setf tmp (- tmp s)
                  (aref qq tmp) i
                  k (aref pp i)
                  s 1)
            (while (> k i)
              (setf (aref qq (+ tmp s)) k
                    k (aref pp k)
                    s (+ s 1)))
            (if (= tmp 0)
                (return-from rep))
           end))
        q)))


(defgeneric permutation-canonical-to-linear (p q)
  (:documentation
   "This function converts a permutation q in canonical form back into
linear form storing it in the output argument p."))

(defmethod permutation-canonical-to-linear ((p permutation) (q permutation))
  (if (not (= (size q) (size p)))
      (error "size of q does not match size of p")
      (let ((n (size p))
            (pp (data p))
            (qq (data 1))
            k kk first)
        (do ((i 0 (+ i 1)))
            ((>= i n))
          (setf (aref pp i) i))
        (setf k (aref qq 0)
              first (aref pp k))
        (do ((i 1 (+ i 1)))
            ((>= i n))
          (setf kk (aref qq i))
          (if (> kk first)
              (setf (aref pp k) (aref pp kk)
                    k kk)
              (setf (aref pp k) first
                    k kk
                    first (aref pp kk)))
          (setf (aref pp k) first))
        p)))


(defgeneric permutation-inversions (p)
  (:documentation
   "This function counts the number of inversions in the permutation
p. An inversion is any pair of elements that are not in order. For
example, the permutation 2031 has three inversions, corresponding to
the pairs (2,0) (2,1) and (3,1). The identity permutation has no
inversions."))

(defmethod permutation-inversions ((p permutation))
  (let ((count 0)
        (size (size p)))
    (do ((i 0 (+ i 1)))
        ((>= i (- size 1)))
      (do ((j (+ i 1) (+ j 1)))
          ((>= j size))
        (if (> (aref (data p) i) (aref (data p) j))
            (incf count))))
    count))


(defgeneric permutation-linear-cycles (p)
  (:documentation
   "This function counts the number of cycles in the permutation p,
given in linear form."))

(defmethod permutation-linear-cycles ((p permutation))
  (let ((count 0) k)
    (do ((i 0 (+ i 1)))
        ((>= i (size p)))
      (setf k (aref (data p) i))
      (while (> k i)
        (setf k (aref (data p) k)))
      (if (< k i)
          (go end))
      (incf count)
     end)
    count))


(defgeneric permutation-canonical-cycles (q)
  (:documentation
   "This function counts the number of cycles in the permutation q,
given in canonical form."))

(defmethod permutation-canonical-cycles ((q permutation))
  (let ((count 1)
        (min (aref (data q) 0)))
    (do ((i 0 (+ i 1)))
        ((>= i (size q)))
      (if (< (aref (data q) i) min)
          (progn
            (setf min (aref (data q) i))
            (incf count))))
    count))
