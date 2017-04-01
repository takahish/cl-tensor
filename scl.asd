(defsystem "scl"
    :description "Scientific Computing Library for Common Lisp"
    :version "0.1"
    :author "Takahiro Ishikawa <takahish.nil@gmail.com>"
    :licence "GNU General Public License version 3"
    :depends-on ("cffi")
    :components ((:module "scl"
                  :components
                  ((:file "pkg")
                   (:file "util")
                   (:file "vector-type")
                   (:file "vector")
                   (:file "matrix-type")
                   (:file "matrix")))))

