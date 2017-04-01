(defsystem "prep"
    :description "Scientific Computing Library for Common Lisp"
    :version "0.1"
    :author "Takahiro Ishikawa <takahish.nil@gmail.com>"
    :licence "GNU General Public License version 3"
    :depends-on ("cffi" "scl" "gsl")
    :components ((:module "prep"
                  :components
                  ((:file "pkg")
                   (:file "data-frame-type")
                   (:file "data-frame")))))
