(require 'str)

(defparameter *input-file* #p"~/Projects/AoC2021/input/input14.txt")
(defun aoc14-load-input ()
  (loop for line in (uiop:read-file-lines *input-file*) collect line))

(defun aoc14-polymer-template (input)
  (first input))

(defun aoc14-polymer-hash (template)
  (let* ((polymer (make-hash-table :test 'equal)))
    (loop for i from 0 below (1- (length template)) do
      (incf (gethash (subseq template i (+ i 2)) polymer 0)))
    polymer))

(defun aoc14-pair-insertions (input)
  (let* ((raw-pairs (remove-if-not #'(lambda (x) (and (not (string= x ""))
						      (char= (char x 3) #\-)))
				   input))
	 (pairs (mapcar #'(lambda (x) (str:split " -> " x)) raw-pairs))
	 (pair-hash (make-hash-table :test 'equal)))
    (loop for pair in pairs do
      (setf (gethash (first pair) pair-hash) (second pair)))
    pair-hash))

(defun insert-pair (pair count insertions polymer)
  (let ((match (gethash pair insertions)))
    (if match
	(let ((new-a (str:concat (str:s-first pair) match))
	      (new-b (str:concat match (str:s-last pair))))
	  (incf (gethash new-a polymer 0) count)
	  (incf (gethash new-b polymer 0) count))
	(incf (gethash pair polymer 0) count))))

(defun insert-pairs (polymer insertions)
  (let ((new-polymer (make-hash-table :test 'equal)))
    (with-hash-table-iterator (poly-iter polymer)
      (loop
	(multiple-value-bind (entry-p k v) (poly-iter)
	  (if entry-p
	      (insert-pair k v insertions new-polymer)
	      (return)))))
    new-polymer))

(defun count-results (polymer template)
  (let ((letters (make-hash-table :test 'equal)))
    (with-hash-table-iterator (poly-iter polymer)
      (loop
	(multiple-value-bind (entry-p k v) (poly-iter)
	  (if entry-p
	      (incf (gethash (str:s-first k) letters 0) v)
	      (return)))))
    (incf (gethash (str:s-last template) letters 0))
    (- (apply #'max (alexandria:hash-table-values letters))
       (apply #'min (alexandria:hash-table-values letters)))))

(defun aoc14 (steps)
  (let* ((input (aoc14-load-input))
	 (template (aoc14-polymer-template input))
	 (polymer (aoc14-polymer-hash template))
	 (insertions (aoc14-pair-insertions input)))
    (dotimes (i steps)
      (setf polymer (insert-pairs polymer insertions)))
    (with-hash-table-iterator (poly-iter polymer)
      (loop
	(multiple-value-bind (entry-p k v) (poly-iter)
	  (if entry-p
	      (format t "~a=~a~%" k v)
	      (return)))))
    (format t "Score: ~a~%" (count-results polymer template))))


