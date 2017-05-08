;;;; blas/blas.lisp
;;;;
;;;; The BLAS (Basic Linear Algebra Subprograms) are routines that
;;;; provide standard building blocks for performing basic vector and
;;;; matrix operations. The Level 1 BLAS perform scalar, vector and
;;;; vector-vector operations, the Level 2 BLAS perform matrix-vector
;;;; operations, and the Level 3 BLAS perform matrix-matrix
;;;; operations. Because the BLAS are efficient, portable, and widely
;;;; available, they are commonly used in the development of high
;;;; quality linear algebra software, LAPACK for example.

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

(cl:in-package "TENSOR")


(defvar *blas-order*
  `((:rowmajor . ,(cffi:foreign-enum-value 'cblas_order :cblasrowmajor))
    (:colmajor . ,(cffi:foreign-enum-value 'cblas_order :cblascolmajor))))

(defvar *blas-transpose*
  `((:notrans . ,(cffi:foreign-enum-value 'cblas_transpose :cblasnotrans))
    (:trans . ,(cffi:foreign-enum-value 'cblas_transpose :cblastrans))
    (:conjtrans . ,(cffi:foreign-enum-value 'cblas_transpose :cblasconjtrans))))

(defvar *blas-uplo*
  `((:upper . ,(cffi:foreign-enum-value 'cblas_uplo :cblasupper))
    (:lower . ,(cffi:foreign-enum-value 'cblas_uplo :cblaslower))))

(defvar *blas-diag*
  `((:nonunit . ,(cffi:foreign-enum-value 'cblas_diag :cblasnonunit))
    (:unit . ,(cffi:foreign-enum-value 'cblas_diag :cblasunit))))

(defvar *blas-side*
  `((:left . ,(cffi:foreign-enum-value 'cblas_side :cblasleft))
    (:right . ,(cffi:foreign-enum-value 'cblas_side :cblasright))))


;;; level 1

(defgeneric blas-dot (x y)
  (:documentation
   "Ths function computes the scalar product xT y for the vectors x
and y."))

(defmacro make-blas-dot (element-type func)
  (let ((vtype (vector-type element-type)))
    `(defmethod blas-dot ((x ,vtype) (y ,vtype))
       (if (= (size x) (size y))
           (cffi:with-pointer-to-vector-data (*x (data x))
             (cffi:with-pointer-to-vector-data (*y (data y))
               (,func (size x) *x (stride x)
                      *y (stride y))))
           (error "invalid length")))))

(make-blas-dot :double cblas_ddot)

(make-blas-dot :float cblas_sdot)


(defgeneric blas-nrm2 (x)
  (:documentation
   "This function computes the Euclidean norm ||x||2 of the vector
x."))

(defmacro make-blas-nrm2 (element-type func)
  (let ((vtype (vector-type element-type)))
    `(defmethod blas-nrm2 ((x ,vtype))
       (cffi:with-pointer-to-vector-data (*x (data x))
         (,func (size x) *x (stride x))))))

(make-blas-nrm2 :double cblas_dnrm2)

(make-blas-nrm2 :float cblas_snrm2)


(defgeneric blas-asum (x)
  (:documentation
   "This function computes the absolute sum |xi| of the elements of the
vector x."))

(defmacro make-blas-asum (element-type func)
  (let ((vtype (vector-type element-type)))
    `(defmethod blas-asum ((x ,vtype))
       (cffi:with-pointer-to-vector-data (*x (data x))
         (,func (size x) *x (stride x))))))

(make-blas-asum :double cblas_dasum)

(make-blas-asum :float cblas_sasum)


(defgeneric blas-iamax (x)
  (:documentation
   "This function returns the index of the largest element of the
vector x. The largest element is determined by its absolute magnitude
for real vectors. If the largest value occurs several times then the
index of the first occurrence is returned."))

(defmacro make-blas-iamax (element-type func)
  (let ((vtype (vector-type element-type)))
    `(defmethod blas-iamax ((x ,vtype))
       (cffi:with-pointer-to-vector-data (*x (data x))
         (,func (size x) *x (stride x))))))

(make-blas-iamax :double cblas_idamax)

(make-blas-iamax :float cblas_isamax)


(defgeneric blas-swap (x y)
  (:documentation
   "This function exchanges the elements of the vectors x and y."))

(defmacro make-blas-swap (element-type func)
  (let ((vtype (vector-type element-type)))
    `(defmethod blas-swap ((x ,vtype) (y ,vtype))
       (if (not (= (size x) (size y)))
           (error "invalid length")
           (cffi:with-pointer-to-vector-data (*x (data x))
             (cffi:with-pointer-to-vector-data (*y (data y))
               (,func (size x) *x (stride x)
                      *y (stride y))
               (values x y)))))))

(make-blas-swap :double cblas_dswap)

(make-blas-swap :float cblas_sswap)


(defgeneric blas-copy (src dest)
  (:documentation
   "This function copies the elements of the vector src into the
vector dest."))

(defmacro make-blas-copy (element-type func)
  (let ((vtype (vector-type element-type)))
    `(defmethod blas-copy ((src ,vtype) (dest ,vtype))
       (if (not (= (size src) (size dest)))
           (error "invalid length")
           (cffi:with-pointer-to-vector-data (*src (data src))
             (cffi:with-pointer-to-vector-data (*dest (data dest))
               (,func (size src) *src (stride src)
                      *dest (stride dest))
               dest))))))

(make-blas-copy :double cblas_dcopy)

(make-blas-copy :float cblas_scopy)


(defgeneric blas-axpy (alpha x y)
  (:documentation
   "This function computes the sum y = alpha * x + y for the vectors x
and y."))

(defmacro make-blas-axpy (element-type func)
  (let ((vtype (vector-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod blas-axpy (alpha (x ,vtype) (y ,vtype))
       (if (not (= (size x) (size y)))
           (error "invalid length")
           (cffi:with-pointer-to-vector-data (*x (data x))
             (cffi:with-pointer-to-vector-data (*y (data y))
               (,func (size x) (coerce alpha (quote ,etype))
                      *x (stride x) *y (stride y))
               y))))))

(make-blas-axpy :double cblas_daxpy)

(make-blas-axpy :float cblas_saxpy)


(defgeneric blas-scal (alpha x)
  (:documentation
   "This function rescales the vector x by the multiplicative factor
alpha."))

(defmacro make-blas-scal (element-type func)
  (let ((vtype (vector-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod blas-scal (alpha (x ,vtype))
       (cffi:with-pointer-to-vector-data (*x (data x))
         (,func (size x) (coerce alpha (quote ,etype))
                *x (stride x)))
       x)))

(make-blas-scal :double cblas_dscal)

(make-blas-scal :float cblas_sscal)


(defun blas-rotg (a b c s)
  "This function computes a Givens rotation (c, s) which zeroes the
vector (a, b)."
  (cffi:with-foreign-objects ((*a :double)
                              (*b :double)
                              (*c :double)
                              (*s :double))
    (setf (cffi:mem-ref *a :double) (coerce a 'double-float)
          (cffi:mem-ref *b :double) (coerce b 'double-float)
          (cffi:mem-ref *c :double) (coerce c 'double-float)
          (cffi:mem-ref *s :double) (coerce s 'double-float))
    (cblas_drotg *a *b *c *s)
    (values (cffi:mem-ref *a :double)
            (cffi:mem-ref *b :double)
            (cffi:mem-ref *c :double)
            (cffi:mem-ref *s :double))))


(defgeneric blas-rot (x y c s)
  (:documentation
   "This function applies a Givens rotation (x′, y′) = (cx + sy, −sx +
cy) to the vectors x, y."))

(defmacro make-blas-rot (element-type func)
  (let ((vtype (vector-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod blas-rot ((x ,vtype) (y ,vtype) c s)
       (if (not (= (size x) (size y)))
           (error "invalid length")
           (cffi:with-pointer-to-vector-data (*x (data x))
             (cffi:with-pointer-to-vector-data (*y (data y))
               (,func (size x) *x (stride x)
                      *y (stride y) (coerce c (quote ,etype))
                      (coerce s (quote ,etype)))
               (values x y)))))))

(make-blas-rot :double cblas_drot)

(make-blas-rot :float cblas_srot)


(defun blas-rotmg (d1 d2 b1 b2 p)
  "This function computes a modified Givens transformation. The
modified Givens transformation is defined in the original Level-1 blas
specification, given in the ref- erences."
  (cffi:with-foreign-objects ((*d1 :double)
                              (*d2 :double)
                              (*b1 :double)
                              (*b2 :double)
                              (*p :double))
    (setf (cffi:mem-ref *d1 :double) (coerce d1 'double-float)
          (cffi:mem-ref *d2 :double) (coerce d2 'double-float)
          (cffi:mem-ref *b1 :double) (coerce b1 'double-float)
          (cffi:mem-ref *b2 :double) (coerce b2 'double-float)
          (cffi:mem-ref *p :double) (coerce p 'double-float))
    (cblas_drotmg *d1 *d2 *b1 *b2 *p)
    (values (cffi:mem-ref *d1 :double)
            (cffi:mem-ref *d2 :double)
            (cffi:mem-ref *b1 :double)
            (cffi:mem-ref *p :double))))


(defgeneric blas-rotm (x y p)
  (:documentation
   "This function applies a modified Givens transformation."))

(defmacro make-blas-rotm (element-type func)
  (let ((vtype (vector-type element-type))
        (etype (vector-element-type element-type)))
    `(defmethod blas-rotm ((x ,vtype) (y ,vtype) p)
       (if (not (= (size x) (size y)))
           (error "invalid length")
           (cffi:with-pointer-to-vector-data (*x (data x))
             (cffi:with-pointer-to-vector-data (*y (data y))
               (,func (size x) *x (stride x)
                      *y (stride y) (coerce p (quote ,etype)))
               (values x y)))))))

(make-blas-rotm :double cblas_drotm)

(make-blas-rotm :float cblas_srotm)


;;; level 2

(defgeneric blas-gemv (a x y &key trans alpha beta)
  (:documentation
   "This function computes the matrix-vector product and sum y = alpha
* op(A) * x + beta * y, where op(A) = A, AT, AH for trans
= :trans, :notrans, :conjtrans."))

(defmacro make-blas-gemv (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type))
        (etype (vector-element-type element-type))
        (one (vector-element-t element-type)))
    `(defmethod blas-gemv ((a ,mtype) (x ,vtype) (y ,vtype)
                           &key (trans :notrans) (alpha ,one) (beta ,one))
       (let ((m (size1 a))
             (n (size2 a)))
         (cond
           ((not (assoc trans *blas-transpose*))
            (error "invalid trans"))
           ((not (or (and (eql trans :notrans)
                          (= n (size x))
                          (= m (size y)))
                     (and (eql trans :trans)
                          (= m (size x))
                          (= n (size y)))))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*x (data x))
                (cffi:with-pointer-to-vector-data (*y (data y))
                  (,func (cdr (assoc :rowmajor *blas-order*))
                         (cdr (assoc trans *blas-transpose*)) (size1 a)
                         (size2 a) (coerce alpha (quote ,etype))
                         *a (tda a) *x (stride x)
                         (coerce beta (quote ,etype)) *y (stride y))
                  y)))))))))

(make-blas-gemv :double cblas_dgemv)

(make-blas-gemv :float cblas_sgemv)


(defgeneric trmv (a x &key trans uplo diag)
  (:documentation
   "This function computes the matrix-vector product x = op(A) * x for
the triangular matrix A, where op(A) = A, AT, AH for trans
= :notrans, :trans, :conjtrans. When uplo is :upper then the upper
triangle of A is used, and when uplo is :lower then the lower triangle
of A is used. If diag is :nonunit then the diagonal of the matrix is
used, but if diag is :unit then the diagonal elements of the matrix A
are taken as unity and are not referenced."))

(defmacro make-blas-trmv (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type)))
    `(defmethod blas-trmv ((a ,mtype) (x ,vtype)
                           &key (trans :notrans) (uplo :upper) (diag :nonunit))
       (let ((m (size1 a))
             (n (size2 a)))
         (cond
           ((not (assoc trans *blas-transpose*))
            (error "invalid trans"))
           ((not (assoc uplo *blas-uplo*))
            (error "invalid uplo"))
           ((not (assoc diag *blas-diag*))
            (error "invalid diag"))
           ((not (= m n))
            (error "matrix must be square"))
           ((not (= n (size x)))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*x (data x))
                (,func (cdr (assoc :rowmajor *blas-order*))
                       (cdr (assoc uplo *blas-uplo*))
                       (cdr (assoc trans *blas-transpose*))
                       (cdr (assoc diag *blas-diag*))
                       (size2 a) *a (tda a)
                       *x (stride x))
                x))))))))

(make-blas-trmv :double cblas_dtrmv)

(make-blas-trmv :float cblas_strmv)


(defgeneric blas-trsv (a x &key trans uplo diag)
  (:documentation
   "This function computes inv(op(A))x for x, where op(A) = A, AT, AH
for trans = :notrans, :trans, :conjtrans. When uplo is :upper then the
upper triangle of A is used, and when uplo is :lower then the lower
triangle of A is used. If diag is :nonunit then the diagonal of the
matrix is used, but if diag is :unit then the diagonal elements of the
matrix A are taken as unity and are not referenced."))

(defmacro make-blas-trsv (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type)))
    `(defmethod blas-trsv ((a ,mtype) (x ,vtype)
                           &key (trans :notrans) (uplo :upper) (diag :nonunit))
       (let ((m (size1 a))
             (n (size2 a)))
         (cond
           ((not (assoc trans *blas-transpose*))
            (error "invalid trans"))
           ((not (assoc uplo *blas-uplo*))
            (error "invalid uplo"))
           ((not (assoc diag *blas-diag*))
            (error "invalid diag"))
           ((not (= m n))
            (error "matrix must be square"))
           ((not (= n (size x)))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*x (data x))
                (,func (cdr (assoc :rowmajor *blas-order*))
                       (cdr (assoc uplo *blas-uplo*))
                       (cdr (assoc trans *blas-transpose*))
                       (cdr (assoc diag *blas-diag*))
                       (size2 a) *a (tda a)
                       *x (stride x))
                x))))))))

(make-blas-trsv :double cblas_dtrsv)

(make-blas-trsv :float cblas_strsv)


(defgeneric blas-symv (a x y &key uplo alpha beta)
  (:documentation
   "Ths function computes the matrix-vector product and sum y = alpha
* A * x + beta * y for the symmetric matrix A. Since the matrix A is
symmetric only its upper half or lower half need to be stored. When
uplo is :upper then the upper triangle and diagonal of A are used, and
when uplo is :lower then the lower triangle and diagonal of A are
used."))

(defmacro make-blas-symv (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type))
        (etype (vector-element-type element-type))
        (one (vector-element-t element-type)))
    `(defmethod blas-symv ((a ,mtype) (x ,vtype) (y ,vtype)
                           &key (uplo :upper) (alpha ,one) (beta ,one))
       (let ((m (size1 a))
             (n (size2 a)))
         (cond
           ((not (assoc uplo *blas-uplo*))
            (error "invalid uplo"))
           ((not (= m n))
            (error "matrix must be square"))
           ((not (or (= n (size x)) (= n (size y))))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*x (data x))
                (cffi:with-pointer-to-vector-data (*y (data y))
                  (,func (cdr (assoc :rowmajor *blas-order*))
                         (cdr (assoc uplo *blas-uplo*))
                         (size2 a) (coerce alpha (quote ,etype))
                         *a (tda a) *x (stride x)
                         (coerce beta (quote ,etype)) *y (stride y))
                  y)))))))))

(make-blas-symv :double cblas_dsymv)

(make-blas-symv :float cblas_ssymv)


(defgeneric blas-ger (x y a &key alpha)
  (:documentation
   "This function computes the rank-1 update A = alpha * x * yT + A of
the matrix A."))

(defmacro make-blas-ger (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type))
        (etype (vector-element-type element-type))
        (one (vector-element-t element-type)))
    `(defmethod blas-ger ((x ,vtype) (y ,vtype) (a ,mtype)
                          &key (alpha ,one))
       (let ((m (size1 a))
             (n (size2 a)))
         (if (not (and (= (size x) m) (= (size y) n)))
             (error "invalid length")
             (cffi:with-pointer-to-vector-data (*a (data a))
               (cffi:with-pointer-to-vector-data (*x (data x))
                 (cffi:with-pointer-to-vector-data (*y (data y))
                   (,func (cdr (assoc :rowmajor *blas-order*))
                          m n (coerce alpha (quote ,etype))
                          *x (stride x) *y (stride y)
                          *a (tda a))
                   a))))))))

(make-blas-ger :double cblas_dger)

(make-blas-ger :float cblas_sger)


(defgeneric blas-syr (x a &key uplo alpha)
  (:documentation
   "This function computes the symmetric rank-1 update A = alpha * x *
xT + A of the symmetric matrix A. Since the matrix A is symmetric only
its upper half or lower half need to be stored. When uplo is :upper
then the upper triangle and diagonal of A are used, and when uplo
is :lower then the lower triangle and diagonal of A are used."))

(defmacro make-blas-syr (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type))
        (etype (vector-element-type element-type))
        (one (vector-element-t element-type)))
    `(defmethod blas-syr ((x ,vtype) (a ,mtype)
                          &key (uplo :upper) (alpha ,one))
       (let ((m (size1 a))
             (n (size2 a)))
         (cond
           ((not (= m n))
            (error "matrix must be sequare"))
           ((not (= (size x) n))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*x (data x))
                (,func (cdr (assoc :rowmajor *blas-order*))
                       (cdr (assoc uplo *blas-uplo*))
                       n (coerce alpha (quote ,etype))
                       *x (stride x) *a (tda a))
                a))))))))

(make-blas-syr :double cblas_dsyr)

(make-blas-syr :float cblas_ssyr)


(defgeneric blas-syr2 (x y a &key uplo alpha)
  (:documentation
   "This function computes the symmetric rank-2 update A = alpha * x *
yT + alpha * y * xT + A of the symmetric matrix A. Since the matrix A
is symmetric only its upper half or lower half need to be stored. When
uplo is :upper then the upper triangle and diagonal of M are used, and
when uplo is :lower then the lower triangle and diagonal of A are
used."))

(defmacro make-blas-syr2 (element-type func)
  (let ((mtype (matrix-type element-type))
        (vtype (vector-type element-type))
        (etype (vector-element-type element-type))
        (one (vector-element-t element-type)))
    `(defmethod blas-syr2 ((x ,vtype) (y ,vtype) (a ,mtype)
                           &key (uplo :upper) (alpha ,one))
       (let ((m (size1 a))
             (n (size2 a)))
         (cond
           ((not (= m n))
            (error "matrix must be square"))
           ((or (not (= (size x) n)) (not (= (size y) n)))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*x (data x))
                (cffi:with-pointer-to-vector-data (*y (data y))
                  (,func (cdr (assoc :rowmajor *blas-order*))
                         (cdr (assoc uplo *blas-uplo*))
                         n (coerce alpha (quote ,etype))
                         *x (stride x) *y (stride y) *a (tda a))
                  a)))))))))

(make-blas-syr2 :double cblas_dsyr2)

(make-blas-syr2 :float cblas_ssyr2)


;;; level 3

(defgeneric blas-gemm (a b c &key transa transb alpha beta)
  (:documentation
   "This function computes the matrix-matrix product and sum C =
alpha * op(A) * op(B) + beta * C where op(A) = A, AT, AH for transa
= :notrans, :trans, :conjtrans and similarly for the parameter
transb."))

(defmacro make-blas-gemm (element-type func)
  (let ((mtype (matrix-type element-type))
        (etype (matrix-element-type element-type))
        (one (matrix-element-t element-type)))
    `(defmethod blas-gemm ((a ,mtype) (b ,mtype) (c ,mtype)
                           &key (transa :notrans) (transb :notrans)
                             (alpha ,one) (beta ,one))
       (let ((m (size1 c))
             (n (size2 c))
             (ma (if (eql transa :notrans) (size1 a) (size2 a)))
             (na (if (eql transa :notrans) (size2 a) (size1 a)))
             (mb (if (eql transa :notrans) (size1 b) (size2 b)))
             (nb (if (eql transa :notrans) (size2 b) (size1 b))))
         (if (not (and (= m ma) (= n nb) (= na mb)))
             (error "invalid length")
             ;; [MxN] = [MAxNA][MBxNB]
             (cffi:with-pointer-to-vector-data (*a (data a))
               (cffi:with-pointer-to-vector-data (*b (data b))
                 (cffi:with-pointer-to-vector-data (*c (data c))
                   (,func (cdr (assoc :rowmajor *blas-order*))
                          (cdr (assoc transa *blas-transpose*))
                          (cdr (assoc transb *blas-transpose*))
                          m n na (coerce alpha (quote ,etype))
                          *a (tda a) *b (tda b)
                          (coerce beta (quote ,etype)) *c (tda c))
                   c))))))))

(make-blas-gemm :double cblas_dgemm)

(make-blas-gemm :float cblas_sgemm)


(defgeneric blas-symm (a b c &key side uplo alpha beta)
  (:documentation
   "These functions compute the matrix-matrix product and sum C =
alpha * A * B + beta * C for side is :left and C = alpha * B * A +
beta * C for side is :right, where the matrix A is symmetric. When
uplo is :upper then the upper triangle and diagonal of A are used, and
when :uplo is :lower then the lower triangle and diagonal of A are
used."))

(defmacro make-blas-symm (element-type func)
  (let ((mtype (matrix-type element-type))
        (etype (matrix-element-type element-type))
        (one (matrix-element-t element-type)))
    `(defmethod blas-symm ((a ,mtype) (b ,mtype) (c ,mtype)
                           &key (side :left) (uplo :upper)
                             (alpha ,one) (beta ,one))
       (let ((m (size1 c))
             (n (size2 c))
             (ma (size1 a))
             (na (size2 a))
             (mb (size1 b))
             (nb (size2 b)))
         (cond
           ((not (= ma na))
            (error "matrix A must be square"))
           ((not (or (and (eql side :left)
                          (and (= m ma) (= n nb) (= na mb)))
                     (and (eql side :right)
                          (and (= m mb) (= n na) (= nb ma)))))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*b (data b))
                (cffi:with-pointer-to-vector-data (*c (data c))
                  (,func (cdr (assoc :rowmajor *blas-order*))
                         (cdr (assoc side *blas-side*))
                         (cdr (assoc uplo *blas-uplo*))
                         m n (coerce alpha (quote ,etype))
                         *a (tda a) *b (tda b)
                         (coerce beta (quote ,etype))
                         *c (tda c))
                  c)))))))))

(make-blas-symm :double cblas_dsymm)

(make-blas-symm :float cblas_ssymm)


(defgeneric blas-trmm (a b &key side uplo trans diag alpha)
  (:documentation
   "This function computes the matrix-matirx product B = alpha * op(A)
* B for side is :left and B = alpha * op(B) * A for side
is :right. The matrix A is triangular and op(A) = A, AT, AH for trans
= :notrans, :trans, :conjtrans. When uplo is :upper then the upper
triangle of A is used, and when uplo is :lower then the lower triangle
of A is used. If diag is :nonunit then the diagonal of A is used, but
if diag is :unit then the diagonal elements of the matrix A are taken
as unity and are not referenced."))

(defmacro make-blas-trmm (element-type func)
  (let ((mtype (matrix-type element-type))
        (etype (matrix-element-type element-type))
        (one (matrix-element-t element-type)))
    `(defmethod blas-trmm ((a ,mtype) (b ,mtype)
                           &key (side :left) (uplo :upper)
                             (trans :notrans) (diag :nonunit) (alpha ,one))
       (let ((m (size1 b))
             (n (size2 b))
             (ma (size1 a))
             (na (size2 a)))
         (cond
           ((not (= ma na))
            (error "matrix A must be square"))
           ((not (or (and (eql side :left) (= m ma))
                     (and (eql side :right) (= n ma))))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*b (data b))
                (,func (cdr (assoc :rowmajor *blas-order*))
                       (cdr (assoc side *blas-side*))
                       (cdr (assoc uplo *blas-uplo*))
                       (cdr (assoc trans *blas-transpose*))
                       (cdr (assoc diag *blas-diag*))
                       m n (coerce alpha (quote ,etype))
                       *a (tda a) *b (tda b))
                b))))))))

(make-blas-trmm :double cblas_dtrmm)

(make-blas-trmm :float cblas_strmm)


(defgeneric blas-trsm (a b &key side uplo trans diag alpha)
  (:documentation
   "This function computes the inverse-matrix matrix product B =
αop(inv(A))B for Side is CblasLeft and B = αBop(inv(A)) for Side is
CblasRight. The matrix A is triangular and op(A) = A, AT , AH for
TransA = CblasNoTrans, CblasTrans, CblasConjTrans. When Uplo is
CblasUpper then the upper triangle of A is used, and when Uplo is
CblasLower then the lower triangle of A is used. If Diag is
CblasNonUnit then the diagonal of A is used, but if Diag is CblasUnit
then the diagonal elements of the matrix A are taken as unity and are
not referenced."))

(defmacro make-blas-trsm (element-type func)
  (let ((mtype (matrix-type element-type))
        (etype (matrix-element-type element-type))
        (one (matrix-element-t element-type)))
    `(defmethod blas-trsm ((a ,mtype) (b ,mtype)
                           &key (side :left) (uplo :upper) (trans :notrans)
                             (diag :nonunit) (alpha ,one))
       (let ((m (size1 b))
             (n (size2 b))
             (ma (size1 a))
             (na (size2 a)))
         (cond
           ((not (= ma na))
            (error "matrix A must be square"))
           ((not (or (and (eql side :left) (= m ma))
                     (and (eql side :right) (= n ma))))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*b (data b))
                (,func (cdr (assoc :rowmajor *blas-order*))
                       (cdr (assoc side *blas-side*))
                       (cdr (assoc uplo *blas-uplo*))
                       (cdr (assoc trans *blas-transpose*))
                       (cdr (assoc diag *blas-diag*))
                       m n (coerce alpha (quote ,etype))
                       *a (tda a) *b (tda b))
                b))))))))

(make-blas-trsm :double cblas_dtrsm)

(make-blas-trsm :float cblas_strsm)


(defgeneric blas-syrk (a c &key uplo trans alpha beta)
  (:documentation
   "This function computes a rank-k update of the symmetric matrix C,
C = alpha * A * AT + beta * C when trans is :notrans and C = alpha *
AT * A + beta * C when trans is :trans. Since the matrix C is
symmetric only its upper half or lower half need to be stored. When
uplo is :upper then the upper triangle and diagonal of C are used, and
when uplo is :lower then the lower triangle and diagonal of C are
used."))

(defmacro make-blas-syrk (element-type func)
  (let ((mtype (matrix-type element-type))
        (etype (matrix-element-type element-type))
        (one (matrix-element-t element-type)))
    `(defmethod blas-syrk ((a ,mtype) (c ,mtype)
                           &key (uplo :upper) (trans :notrans)
                             (alpha ,one) (beta ,one))
       (let ((m (size1 c))
             (n (size2 c))
             (j (if (eql trans :notrans) (size1 a) (size2 a)))
             (k (if (eql trans :notrans) (size2 a) (size1 a))))
         (cond
           ((not (= m n))
            (error "matrix C must be square"))
           ((not (= n j))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*c (data c))
                (,func (cdr (assoc :rowmajor *blas-order*))
                       (cdr (assoc uplo *blas-uplo*))
                       (cdr (assoc trans *blas-transpose*))
                       n k (coerce alpha (quote ,etype))
                       *a (tda a) (coerce beta (quote ,etype))
                       *c (tda c))
                c))))))))

(make-blas-syrk :double cblas_dsyrk)

(make-blas-syrk :float cblas_ssyrk)


(defgeneric blas-syr2k (a b c &key uplo trans alpha beta)
  (:documentation
   "This function computes a rank-2k update of the symmetric matrix C,
C = alpha * A * BT + alpha * B * AT + beta * C when trans is :notrans
and C = alpha * AT * B + alpha * BT * A + beta * C when trans
is :trans. Since the matrix C is symmetric only its upper half or
lower half need to be stored. When uplo is :upper then the upper
triangle and diagonal of C are used, and when uplo is :lower then the
lower triangle and diagonal of C are used."))

(defmacro make-blas-syr2k (element-type func)
  (let ((mtype (matrix-type element-type))
        (etype (matrix-element-type element-type))
        (one (matrix-element-t element-type)))
    `(defmethod blas-syr2k ((a ,mtype) (b ,mtype) (c ,mtype)
                            &key (uplo :upper) (trans :notrans)
                              (alpha ,one) (beta ,one))
       (let ((m (size1 c))
             (n (size2 c))
             (ma (if (eql trans :notrans) (size1 a) (size2 a)))
             (na (if (eql trans :notrans) (size2 a) (size1 a)))
             (mb (if (eql trans :notrans) (size1 b) (size2 b)))
             (nb (if (eql trans :notrans) (size2 b) (size1 b))))
         (cond
           ((not (= m n))
            (error "matrix C must be square"))
           ((or (not (= n ma)) (not (= n mb)) (not (= na nb)))
            (error "invalid length"))
           (t
            (cffi:with-pointer-to-vector-data (*a (data a))
              (cffi:with-pointer-to-vector-data (*b (data b))
                (cffi:with-pointer-to-vector-data (*c (data c))
                  (,func (cdr (assoc :rowmajor *blas-order*))
                         (cdr (assoc uplo *blas-uplo*))
                         (cdr (assoc trans *blas-transpose*))
                         n na (coerce alpha (quote ,etype))
                         *a (tda a) *b (tda b)
                         (coerce beta (quote ,etype)) *c (tda c))
                  c)))))))))

(make-blas-syr2k :double cblas_dsyr2k)

(make-blas-syr2k :float cblas_ssyr2k)
