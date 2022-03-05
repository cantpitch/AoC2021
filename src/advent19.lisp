(ql:quickload "str")

(load #p"~/Projects/AoC2021/src/helper.lisp")

(defparameter *input-file* #p"~/Projects/Aoc2021/input/input19.txt")

(defun aoc19-load-input ()
  (let* ((input (load-input *input-file*))
	 (scanners (make-hash-table :test 'equal))
	 (scanner-num -1))
    (loop for line in input do
      (cond ((string= (str:trim line) "") nil)
	    ((string= (str:substring 0 3 line) "---")
	     (progn
	       (incf scanner-num)
	       (setf (gethash scanner-num scanners) )))))))
