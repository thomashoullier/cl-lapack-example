;;;; Example: Calling the foreign function dgemm from lapack/blas.

(declaim (inline dgemm))

;; LAPACK/BLAS must be installed for this to load of course.
(load-shared-object "libblas.so.3")

;; Defining the lisp function dgemm from the blas "dgemm_"
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
  (sap-alien (sb-sys:vector-sap (array-storage-vector array)) (* double)))

(let ((a (make-array '(2 3) :element-type 'double-float
			    :initial-contents '((2d0 1d0 6d0) (7d0 3d0 4d0))))
      (b (make-array '(3 2) :element-type 'double-float
			    :initial-contents '((3d0 1d0) (6d0 5d0) (2d0 3d0))))
      (c (make-array '(2 2) :element-type 'double-float))
      (expected-c (make-array '(2 2) :element-type 'double-float
			      :initial-contents '((24d0 25d0) (47d0 34d0)))))
  (sb-sys:with-pinned-objects (a b c)
    (dgemm "n" "n" 2 2 3 1d0 (pointer b) 2 (pointer a) 3 0d0 (pointer c) 2))
  (format t "~A~%~A~%" expected-c c))
