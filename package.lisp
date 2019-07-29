(defpackage :dgemm
  (:documentation "LAPACK dgemm SBCL call.")
  (:use :cl :sb-alien)
  (:export #:mm-lapack #:mm))
