;;;; dgemm call using SBCL alien interface.

(in-package :dgemm)
(declaim (inline dgemm mm-lapack pointer))
(sb-alien:load-shared-object "libblas.so.3")

(define-alien-routine ("dgemm_" dgemm) void
  (transa c-string)
  (transb c-string)
  (m int :copy)
  (n int :copy)
  (k int :copy)
  (alpha double :copy)
  (a (* double))
  (lda int :copy)
  (b (* double))
  (ldb int :copy)
  (beta double :copy)
  (c (* double))
  (ldc int :copy))

(defun pointer (array)
  (sap-alien (sb-sys:vector-sap
	      (sb-ext:array-storage-vector array)) (* double)))

(defun mm-lapack (a b c)
  (let* ((a0 (array-dimension a 0))
	 (a1 (array-dimension a 1))
	 (b1 (array-dimension b 1)))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm "n" "n" b1 a0 a1 1d0
	     (pointer b) b1 (pointer a) a1 0d0 (pointer c) b1))))
