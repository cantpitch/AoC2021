(require 'str)
(require 'alexandria)

(defun aoc09-load-input ()
  (let* ((raw (uiop:read-file-lines #p"~/Projects/AoC2021/input/input09.txt"))
	 (world (make-array (list (length raw) (length (car raw))))))
    (loop for row in raw
	  for y from 0 do
	    (loop for col across row
		  for x from 0 do
		  (setf (aref world y x) (digit-char-p col))))
    world))

(defun is-lower (world x1 y1 x2 y2)
  (if (or (< x2 0)
	  (< y2 0)
	  (>= x2 (array-dimension world 1))
	  (>= y2 (array-dimension world 0)))
      t
      (< (aref world y1 x1) (aref world y2 x2))))

(defun is-lower-than-neighbors (world x y)
  (and (is-lower world x y (1- x) y)
       (is-lower world x y x (1- y))
       (is-lower world x y (1+ x) y)
       (is-lower world x y x (1+ y))))

(defun aoc0901 ()
  (let* ((world (aoc09-load-input))
	 (total 0))
    (loop for y from 0 below (array-dimension world 0) do
      (loop for x from 0 below (array-dimension world 1) do
	(when (is-lower-than-neighbors world x y)
	  (incf total (1+ (aref world y x))))))
    total))

(defun basin-size (world x y)
  (when (or (< x 0)
	    (< y 0)
	    (>= x (array-dimension world 1))
	    (>= y (array-dimension world 0))
	    (equal (aref world y x) #\X)
	    (equal (aref world y x) 9))
    (return-from basin-size 0))
  (let ((size 1))
    (setf (aref world y x) #\X)
    (incf size (basin-size world (1- x) y))
    (incf size (basin-size world x (1- y)))
    (incf size (basin-size world (1+ x) y))
    (incf size (basin-size world x (1+ y)))
    size))

(defun aoc0902 ()
  (let* ((world (aoc09-load-input))
	 (basins nil))
    (loop for y from 0 below (array-dimension world 0) do
      (loop for x from 0 below (array-dimension world 1) do
	(when (is-lower-than-neighbors world x y)
	  (push (list x y) basins))))
    (reduce #'*
	    (subseq
	     (sort
	      (map 'list
		   #'(lambda (b) (basin-size world (first b) (second b)))
		   basins)
	      #'>)
	     0 3)))))
