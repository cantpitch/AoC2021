(require 'str)

(defun aoc06-load-input ()
  (car (uiop:read-file-lines #p"~/Projects/AoC2021/input/input06.txt")))

(defun aoc06-parse-input (input)
  (let ((initial-fish (map 'list #'parse-integer (str:split "," input)))
	(fish-hash (make-hash-table)))
    (dotimes (i 9)
      (setf (gethash i fish-hash) 0))
    (dolist (fish initial-fish)
      (incf (gethash fish fish-hash)))
    fish-hash))

(defun aoc06-sum-results (fish-hash)
  (let ((sum 0))
    (dotimes (i 9)
      (incf sum (gethash i fish-hash)))
    sum))

(defun aoc0601 (days)
  (let* ((fish (aoc06-parse-input (aoc06-load-input))))
    (loop for day from 1 to days do
      (let ((new-fish (make-hash-table)))
	(format t "Day ~a ~% " day)
	(loop for i from 8 downto 1 do
	  (setf (gethash (1- i) new-fish) (gethash i fish)))
	(setf (gethash 8 new-fish) 0)
	(let ((birthing (gethash 0 fish)))
	  (when (> birthing 0)
	    (incf (gethash 6 new-fish) birthing)
	    (incf (gethash 8 new-fish) birthing)))
	(setf fish new-fish)))
     (aoc06-sum-results fish)))
