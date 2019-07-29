;;;; Pure CL implementations of dense matrix multiplication for comparison.

(in-package :dgemm)
(declaim (inline mm mm-spec))

(defun mm (a b c)
  "Matrix multiplication. C = A.B"
  (let ((a0 (array-dimension a 0))
	(a1 (array-dimension a 1))
	(b1 (array-dimension b 1))
	(s 0))
    (loop for i from 0 below a0 do
      (loop for j from 0 below b1 do
	(setf s 0)
	(loop for k from 0 below a1 do
	  (incf s (* (aref a i k) (aref b k j))))
	(setf (aref c i j) s)))))

(defun mm-spec (a b c)
  "Specialized matrix multiplication for comparison."
  (declare (optimize (speed 3))
	   (type (simple-array double-float (* *)) a b c))
  (let ((a0 (array-dimension a 0))
	(a1 (array-dimension a 1))
	(b1 (array-dimension b 1))
	(s 0d0))
    (declare (type double-float s))
    (loop for i from 0 below a0 do
      (loop for j from 0 below b1 do
	(setf s 0d0)
	(loop for k from 0 below a1 do
	  (incf s (* (aref a i k) (aref b k j))))
	(setf (aref c i j) s)))))
