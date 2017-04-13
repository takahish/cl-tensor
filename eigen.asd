(defsystem "eigen"
    :description "a common lisp library for linear algebra"
    :version "0.1"
    :author "Takahiro Ishikawa <takahish.nil@gmail.com>"
    :licence "The MIT License"
    :depends-on ("cffi")
    :components ((:file "pkg")
                 (:file "util")
                 (:file "vector-type")
                 (:file "vector")
                 (:file "matrix-type")
                 (:file "matrix")
                 (:module "blas"
                  :components
                  ((:module "if"
                    :components
                    ((:file "cblas")))
                   (:file "blas")))))
