(require 'str)
(require 'alexandria)
(use-package 'alexandria)
(use-package 'cl-ppcre)

(defparameter *input-file* #p"~/Projects/AoC2021/input/input13.txt")

(defun aoc13-load-input ()
  (loop for line in (uiop:read-file-lines *input-file*) collect line))

(defun aoc13-coords (input)
  (let* ((coords (remove-if #'(lambda (x) (or (string= x "")
					      (string= (subseq x 0 1) "f")))
			    input)))
    (loop for coord in coords
	  collect
	  (let ((parsed (str:split "," coord)))
	    (list (parse-integer (first parsed))
		  (parse-integer (second parsed)))))))

(defun aoc13-folds (input)
  (let* ((folds 
	   (remove-if-not #'(lambda (x) (and (> (length x) 0)
					     (string= (subseq x 0 1) "f")))
			  input)))
    (loop for fold in folds
	  collect
	  (let ((parsed (str:split "=" (scan-to-strings "[xy]=[0-9]+" fold))))
	    (list (first parsed) (parse-integer (second parsed)))))))

(defun build-paper-from-coords (coords)
  (let* ((max-x (1+ (apply #'max (mapcar #'first coords))))
	 (max-y (1+ (apply #'max (mapcar #'second coords))))
	 (paper (make-array (list max-y max-x) :initial-element "." :adjustable t)))
    (loop for coord in coords do
      (setf (aref paper (second coord) (first coord)) "#"))
    paper))

(defun fold-paper-y (paper y-fold)
  (loop for y from (1+ y-fold) below (array-dimension paper 0)
	for yi from 1 do
	  (loop for x from 0 below (array-dimension paper 1) do
	    (when (string= (aref paper y x) "#")
	      (setf (aref paper (- y-fold yi) x) "#"))))
  (adjust-array paper (list y-fold (array-dimension paper 1))))

(defun fold-paper-x (paper x-fold)
  (loop for y from 0 below (array-dimension paper 0) do
    (loop for x from (1+ x-fold) below (array-dimension paper 1)
	  for xi from 1 do
	    (when (string= (aref paper y x) "#")
	      (setf (aref paper y (- x-fold xi)) "#"))))
  (adjust-array paper (list (array-dimension paper 0) x-fold)))

(defun count-dots (paper)
  (let ((sum 0))
    (loop for y from 0 below (array-dimension paper 0) do
      (loop for x from 0 below (array-dimension paper 1) do
	(when (string= (aref paper y x) "#") (incf sum))))
    sum))

(defun aoc1301 ()
  (let* ((input (aoc13-load-input))
	 (coords (aoc13-coords input))
	 (folds (aoc13-folds input))
	 (paper (build-paper-from-coords coords))
	 (first-fold (first folds)))
    (if (string= (first first-fold) "x")
	(fold-paper-x paper (second first-fold))
	(fold-paper-y paper (second first-fold)))
    (format t "~a" paper)
    (count-dots paper)))

(defun print-2d-array (array)
  (loop for y from 0 below (array-dimension array 0) do
    (loop for x from 0 below (array-dimension array 1) do
      (format t "~a" (aref array y x)))
    (format t "~%")))

(defun aoc1302 ()
  (let* ((input (aoc13-load-input))
	 (coords (aoc13-coords input))
	 (folds (aoc13-folds input))
	 (paper (build-paper-from-coords coords)))
    (loop for fold in folds do
      (if (string= (first fold) "x")
	  (fold-paper-x paper (second fold))
	  (fold-paper-y paper (second fold))))
    (print-2d-array paper)))
