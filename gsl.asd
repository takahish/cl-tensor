(defsystem "gsl"
    :description "Wrapper for GUN Scientific Library"
    :version "0.1"
    :author "Takahiro Ishikawa <takahish.nil@gmail.com>"
    :licence "GNU General Public License version 3"
    :depends-on ("cffi" "scl")
    :components ((:module "gsl"
                  :components
                  ((:file "pkg")
                   (:module "if"
                    :components
                    ((:file "gsl_rng")
                     (:file "gsl_qrng")
                     (:file "gsl_randist")
                     (:file "gsl_vector")
                     (:file "gsl_matrix")
                     (:file "gsl_sort")
                     (:file "gsl_stats")))
                   (:file "rng")
                   (:file "qrng")
                   (:file "randist")
                   (:file "vector")
                   (:file "matrix")
                   (:file "sort")
                   (:file "stats")))))
