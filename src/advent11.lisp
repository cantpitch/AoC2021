(defun aoc11-load-input ()
  (let* ((raw (uiop:read-file-lines #p"~/Projects/AoC2021/input/input11.txt"))
	 (world (make-array (list (length raw) (length (car raw))))))
    (loop for row in raw
	  for y from 0 do
	    (loop for col across row
		  for x from 0 do
		  (setf (aref world y x) (digit-char-p col))))
    world))

(defun increment-all (world)
  (loop for y from 0 below (array-dimension world 0) do
    (loop for x from 0 below (array-dimension world 1) do
      (incf (aref world y x)))))

(defun flashable-p (world)
  (loop for y from 0 below (array-dimension world 0) do
    (loop for x from 0 below (array-dimension world 1) do
      (when (> (aref world y x) 9)
	(return-from flashable-p t))))
  nil)

(defun increment-cell (world x y)
  (when (and (>= x 0) (< x (array-dimension world 1))
	   (>= y 0) (< y (array-dimension world 0))
	   (/= (aref world y x) 0))
      (incf (aref world y x))))

(defun flash-cell (world x y)
  (when (<= (aref world y x) 9) (return-from flash-cell))
  (setf (aref world y x) 0)
  (increment-cell world (1- x) (1- y))
  (increment-cell world x (1- y))
  (increment-cell world (1+ x) (1- y))
  (increment-cell world (1- x) y)
  (increment-cell world (1+ x) y)
  (increment-cell world (1- x) (1+ y))
  (increment-cell world x (1+ y))
  (increment-cell world (1+ x) (1+ y)))

(defun count-flashes (world)
  (let ((total 0))
    (loop for y from 0 below (array-dimension world 0) do
      (loop for x from 0 below (array-dimension world 1) do
	(when (= (aref world y x) 0) (incf total))))
    total))

(defun flash (world)
  (loop while (flashable-p world) do
    (loop for y from 0 below (array-dimension world 0) do
      (loop for x from 0 below (array-dimension world 1) do
	(when (> (aref world y x) 9) (flash-cell world x y))))))

(defun aoc1101 (steps)
  (let ((world (aoc11-load-input))
	(total 0))
    (loop for step from 1 to steps do
      (increment-all world)
      (flash world)
      (incf total (count-flashes world)))
    (format t "Total flashes after ~a steps: ~a~%" steps total)
    world))

(defun aoc1102 ()
  (let ((world (aoc11-load-input))
	(step 0))
    (loop while (< (count-flashes world) 100) do
      (increment-all world)
      (flash world)
      (incf step))
    (format t "All flashed after ~a steps.~%" step)
    world))
