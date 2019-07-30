;;;; Testing and comparing the matrix multiplication implementations.

;;; Helper
(defun checkm (mat1 mat2)
  "Assert that the two matrices are identical."
  (let ((i1 (array-dimension mat1 0))
	(i2 (array-dimension mat2 0))
	(j1 (array-dimension mat1 1))
	(j2 (array-dimension mat2 1)))
    (assert (and (= i1 i2) (= j1 j2)))
    (loop for i from 0 below i1 do
      (loop for j from 0 below j1 do
	(assert (= (aref mat1 i j) (aref mat2 i j)))))))

(defmacro internal-time (body)
  "Time the execution of 'body' and return the result in seconds.
1. Real time.
2. Run time."
  `(let ((start-run-time (get-internal-run-time))
	 (start-real-time (get-internal-real-time)))
     ,body
     (values (/ (- (get-internal-real-time) start-real-time)
		internal-time-units-per-second)
	     (/ (- (get-internal-run-time) start-run-time)
		internal-time-units-per-second))))

;;; Example test
(let ((a (make-array '(2 3) :element-type 'double-float
			    :initial-contents '((2d0 1d0 6d0) (7d0 3d0 4d0))))
      (b (make-array '(3 2) :element-type 'double-float
			    :initial-contents '((3d0 1d0) (6d0 5d0) (2d0 3d0))))
      (c (make-array '(2 2) :element-type 'double-float))
      (expected-c (make-array '(2 2) :element-type 'double-float
			      :initial-contents '((24d0 25d0) (47d0 34d0)))))
  (dgemm:mm-lapack a b c)
  (checkm c expected-c))

(format t "Example test: PASSED.~%")

;;; Stochastic test of LAPACK vs. pure CL.
(let ((ntests 10)
      (maxdim 10)
      (ampl 1000d0))
  (dotimes (it ntests)
    (let* ((idim (+ 1 (random maxdim)))
	   (jdim (+ 1 (random maxdim)))
	   (idim2 jdim)
	   (jdim2 (+ 1 (random maxdim)))
	   (a (make-array (list idim jdim) :element-type 'double-float))
	   (b (make-array (list idim2 jdim2) :element-type 'double-float))
	   (c1 (make-array (list idim jdim2) :element-type 'double-float))
	   (c2 (make-array (list idim jdim2) :element-type 'double-float)))
      ;; Filling the matrices with random values.
      (loop for mat in (list a b) do
	(loop for i from 0 below (array-dimension mat 0) do
	  (loop for j from 0 below (array-dimension mat 1) do
	    (setf (aref mat i j) (- (random ampl) (/ ampl 2d0))))))
      (dgemm:mm-lapack a b c1)
      (dgemm:mm a b c2)
      (checkm c1 c2))))

(format t "Stochastic test: PASSED.~%")

;;; Speed information
