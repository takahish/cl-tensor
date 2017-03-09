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
                     (:file "gsl_stats")
                     (:file "gsl_histogram")))
                   (:file "rng-type")
                   (:file "rng")
                   (:file "qrng-type")
                   (:file "qrng")
                   (:file "randist")
                   (:file "vector-type")
                   (:file "vector")
                   (:file "matrix-type")
                   (:file "matrix")
                   (:file "sort")
                   (:file "stats")
                   (:file "histogram-type")
                   (:file "histogram")))))
