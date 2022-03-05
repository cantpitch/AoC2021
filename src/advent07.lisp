(require 'str)

(defun aoc07-load-input ()
  (car (uiop:read-file-lines #p"~/Projects/AoC2021/input/input07.txt")))

(defun aoc07-parse-input (input)
  (map 'list #'parse-integer (str:split "," input)))

(defun aoc0701-cost (distance) distance)

(defun aoc0702-cost (distance)
  (let ((sum 0))
    (loop for i from 1 to distance do
      (incf sum i))
    sum))

(defun aoc07 (cost-fn)
  (let* ((input (aoc07-parse-input (aoc07-load-input)))
	 (max-pos (apply #'max input))
	 (costs (make-array (1+ max-pos) :initial-element 0)))
    (loop for i from 0 to max-pos do
      (dolist (crab input)
	(incf (aref costs i) (funcall cost-fn (abs (- i crab))))))
    (reduce #'min costs)))

