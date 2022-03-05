(defparameter *target-area* '((244 303) (-91 -54)))

(defclass point () ((x :initarg :x :accessor point-x) 
		    (y :initarg :y :accessor point-y)))

(defmethod print-object ((p point) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "x: ~a, y: ~a" (point-x p) (point-y p))))

(defclass probe () ((position
		     :initarg :position
		     :initform (make-instance 'point :x 0 :y 0)
		     :accessor probe-position)
		    (velocity :initarg :velocity :accessor probe-velocity)
		    (step :initform 0 :accessor probe-step)
		    (max-y :initform 0 :accessor probe-max-y)))

(defmethod print-object ((p probe) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (format stream "position: ~a, velocity: ~a"
	    (probe-position p) (probe-velocity p))))

(defmethod fire-probe ((p probe))
  (incf (point-x (probe-position p)) (point-x (probe-velocity p)))
  (incf (point-y (probe-position p)) (point-y (probe-velocity p)))
  (when (> (point-y (probe-position p)) (probe-max-y p))
    (setf (probe-max-y p) (point-y (probe-position p))))
  (decf (point-y (probe-velocity p)) 1)
  (cond ((> (point-x (probe-velocity p)) 0) (decf (point-x (probe-velocity p))))
	((< (point-x (probe-velocity p)) 0) (incf (point-x (probe-velocity p)))))
  (incf (probe-step p))
  ;;(pprint p)
  p)

(defmethod in-target-area-p ((p probe))
  (and (>= (point-x (probe-position p)) (caar *target-area*))
       (<= (point-x (probe-position p)) (cadar *target-area*))
       (>= (point-y (probe-position p)) (caadr *target-area*))
       (<= (point-y (probe-position p)) (cadadr *target-area*))))

(defmethod past-target-area-p ((p probe))
  (or (> (point-x (probe-position p)) (cadar *target-area*))
      (< (point-y (probe-position p)) (caadr *target-area*))))

(defun aoc17 (min-x max-x min-y max-y return-fn)
  (let ((hits nil))
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
	(do ((probe (make-instance 'probe
				   :velocity (make-instance 'point :x x :y y))
		    (fire-probe probe)))
	    ((past-target-area-p probe))
	  (when (in-target-area-p probe)
	    (push (list x y (probe-max-y probe)) hits)
	    (return)))))
    (funcall return-fn hits)))

(defun aoc1701-max-y (hits)
  (reduce #'(lambda (a b) (if (> (third a) (third b)) a b))
	    hits))

(defun aoc1702-total (hits)
  (length hits))

(defun aoc1702-debug (hits)
  (print hits))

(defun aoc1701 () (aoc17 100 100 #'aoc1701-max-y))
(defun aoc1702 () (aoc17 -100 304 -100 100 #'aoc1702-total))


