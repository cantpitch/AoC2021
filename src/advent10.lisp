(defun aoc10-load-input ()
  (let* ((input
	   (uiop:read-file-lines #p"~/Projects/AoC2021/input/input10.txt")))
    (loop for line in input
	  collect (loop for c across line collect c))))

(defvar *illegal-score* #. (let ((ht (make-hash-table)))
			     (loop for (k . v) in
				   '((#\) . 3) (#\] . 57)
				     (#\} . 1197) (#\> . 25137))
				   do (setf (gethash k ht) v))
			     ht))

(defvar *completion-score* #. (let ((ht (make-hash-table)))
			     (loop for (k . v) in
				   '((#\) . 1) (#\] . 2)
				     (#\} . 3) (#\> . 4))
				   do (setf (gethash k ht) v))
			     ht))


(defun close-bracket-p (bracket)
  (or (eql bracket #\]) (eql bracket #\}) (eql bracket #\)) (eql bracket #\>)))

(defun open-bracket-p (bracket)
  (or (eql bracket #\[) (eql bracket #\{) (eql bracket #\() (eql bracket #\<)))

(defun proper-close-bracket-p (open-bracket close-bracket)
  (or (and (eql open-bracket #\[) (eql close-bracket #\]))
      (and (eql open-bracket #\() (eql close-bracket #\)))
      (and (eql open-bracket #\{) (eql close-bracket #\}))
      (and (eql open-bracket #\<) (eql close-bracket #\>))))

(defun close-bracket (bracket)
  (cond ((eql bracket #\[) #\])
	((eql bracket #\() #\))
	((eql bracket #\{) #\})
	((eql bracket #\<) #\>)))

(defun aoc1001-parse-line (line)
  (let* ((stack nil))
    (dolist (bracket line)
      (if (open-bracket-p bracket)
	  (push bracket stack)
	  (let ((open-bracket (pop stack)))
	    (when (not (proper-close-bracket-p open-bracket bracket))
	      (format t "- Expected ~a, but found ~a instead.~%"
		      (close-bracket open-bracket) bracket)
	      (return-from aoc1001-parse-line (gethash bracket *illegal-score*))))))
    0))

(defun aoc1002-parse-line (line)
  (let* ((stack nil))
    (dolist (bracket line)
      (if (open-bracket-p bracket)
	  (push bracket stack)
	  (let ((open-bracket (pop stack)))
	    (when (not (proper-close-bracket-p open-bracket bracket))
	      (format t "- Expected ~a, but found ~a instead.~%"
		      (close-bracket open-bracket) bracket)
	      (return-from aoc1002-parse-line 0)))))
    (let ((total 0)
	  (closes nil))
      (dolist (bracket stack)
	(push (close-bracket bracket) closes)
	(setf total (+ (* 5 total)
		       (gethash (close-bracket bracket) *completion-score*))))
      (when (> total 0)
	(format t "- Complete by adding ~{~A~}. = ~a~%" closes total))
      total)))

(defun aoc1001 ()
  (let ((input (aoc10-load-input))
	(total 0))
    (dolist (line input)
      (incf total (aoc1001-parse-line line)))
    total))


(defun aoc1002 ()
  (let ((input (aoc10-load-input))
	(scores nil))
    (dolist (line input)
      (let ((score 0))
	(setf score (aoc1002-parse-line line))
	(when (> score 0) (push score scores))))
    (setf scores (sort scores #'>))
    (nth (floor (/ (length scores) 2)) scores)))
