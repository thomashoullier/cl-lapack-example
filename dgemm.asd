(defsystem dgemm
  :name "dgemm"
  :version "0.1"
  :author "karl"
  :description "LAPACK dgemm sbcl calls."
  :components ((:file "package")
	       (:file "dgemm" :depends-on ("package"))))
