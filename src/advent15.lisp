(load #P"~/Projects/AoC2021/src/helper.lisp")

(defparameter *input-file* #p"~/Projects/AoC2021/input/input15.txt")

(defun aoc15-load-input () (input-to-2d-array (load-input *input-file*)))

(defun rot-height (val) (if (> val 8) 1 (1+ val)))

(defun aoc15-dupe-input (input)
  (let* ((dims (array-dimensions input))
	(orig-y (first dims))
	(orig-x (second dims)))
    (adjust-array input (list (* 5 orig-y) (* 5 orig-x)))
    (loop for x-grid from 1 to 4 do
      (loop for y from 0 below orig-x do
	(loop for x from 0 below orig-y do
	  (let ((new-val (rot-height (aref input y (+ (* (1- x-grid) orig-x) x)))))
	    (setf (aref input y (+ (* x-grid orig-x) x)) new-val)))))
    (loop for y-grid from 1 to 4 do
      (loop for x-grid from 0 to 4 do
	(loop for y from 0 below orig-x do
	  (loop for x from 0 below orig-y do
	    (let ((new-val (rot-height (aref input
					     (+ (* (1- y-grid) orig-y) y)
					     (+ (* x-grid orig-x) x)))))
	      (setf (aref input
			  (+ (* y-grid orig-y) y)
			  (+ (* x-grid orig-x) x))
		    new-val))))))
    input))

(defun aoc15-build-visited (x-max y-max)
  (let ((visited (make-hash-table :test 'equal)))
    (loop for y from 0 below y-max do
      (loop for x from 0 below x-max do
	(setf (gethash (cons x y) visited) 1)))
    visited))

(defun min-distance (dists Q)
  (let ((curr 10000)
	(pos '(0 . 0)))
    (loop for vert in (hash-table-keys Q) do
      (let ((x (car vert))
	    (y (cdr vert)))
	(when (< (aref dists y x) curr)
	  (setf curr (aref dists y x)
		pos (cons x y)))))
    (values pos curr)))

(defun neighbors (point Q)
  (let* ((x (car point))
	 (y (cdr point))
	 (ns (list (cons x (1- y)) (cons (1+ x) y)
		   (cons x (1+ y)) (cons (1- x) y))))
    (setf ns (remove-if #'(lambda (a) (not (gethash a Q)))
			ns))
    ns))

(defun aoc15 (input)
  (let* ((dists (make-array (array-dimensions input) :initial-element 10000))
	 (prev (make-array (array-dimensions input) :initial-element nil))
	 (Q (aoc15-build-visited (array-dimension input 1)
				 (array-dimension input 0))))
    (setf (aref dists 0 0) 0)
    (loop while (> (hash-table-count Q) 0) do
      (let ((u (min-distance dists Q)))
	(when (= (mod (hash-table-count Q) 100) 0)
	  (format t "~A " (hash-table-count Q)))
	(remhash u Q)
	(loop for v in (neighbors u Q) do
	  (let ((alt (+ (aref dists (cdr u) (car u))
			(aref input (cdr v) (car v)))))
	    (when (< alt (aref dists (cdr v) (car v)))
	      (setf (aref dists (cdr v) (car v)) alt
		    (aref prev (cdr v) (car v)) u))))))
    (aref dists
	  (1- (array-dimension dists 0))
	  (1- (array-dimension dists 1)))))
