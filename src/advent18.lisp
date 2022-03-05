(ql:quickload 'alexandria)
(ql:quickload 'str)
(load #p"~/Projects/AoC2021/src/helper.lisp")


(defclass sn () ((left :initarg :left :accessor left)
		 (right :initarg :right :accessor right)))
(defclass num () ((num :initarg :num :accessor num)))

(defmethod print-object ((obj sn) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "[~a,~a]" (left obj) (right obj))))

(defmethod print-object ((obj num) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (num obj))))

(defmethod print-snailfish (n)
  (if (num? n) (format nil "~A" (num n))
      (format nil "[~A,~A]" (print-snailfish (left n))
	      (print-snailfish (right n)))))

(defun list-to-snailfish (l)
  (if (numberp l)
      (sn-num l)
      (sn (list-to-snailfish (car l))
	  (list-to-snailfish (cdr l)))))

(defun parse1 (s)
  (list-to-snailfish
   (read-from-string
    (str:replace-using (list "[" "( "
			     "]" ")"
			     "," " . ")
		       s))))

(defun parse (l)
  (loop for s in l
	collect (parse1 s)))


(defun num? (n) (typep n 'num))

(defmethod leaf? ((n sn)) (and (num? (left n)) (num? (right n))))

(defun sn (l r) (make-instance 'sn :left l :right r))

(defun sn-num (n) (make-instance 'num :num n))

(defun magnitude-left (n)
  (* 3 (if (num? n) (num n) (magnitude n))))

(defun magnitude-right (n)
  (* 2 (if (num? n) (num n) (magnitude n))))

(defun magnitude (n)
  (if (num? n)
      (num n)
      (+ (magnitude-left (left n)) (magnitude-right (right n)))))

(defun add (na nb) (sn na nb))

(defun split (n)
  (if (num? n)
      (if (> (num n) 9)
	  (values (sn (sn-num (floor (/ (num n) 2)))
		      (sn-num (ceiling (/ (num n) 2))))
		  t)
	  (values (num n) nil))
      (multiple-value-bind (l did-split-l?) (split (left n))
	(if did-split-l?
	    (values (sn l (right n)) t)
	    (multiple-value-bind (r did-split-r?) (split (right n))
	      (if did-split-r?
		  (values (sn (left n) r) t)
		  (values n nil)))))))

(defun explode-traverse (n depth traversal-list explode)
  (when (typep n 'sn)
    (multiple-value-bind (new-tlist new-explode)
	(explode-traverse (left n) (1+ depth) traversal-list explode)
      (setf traversal-list new-tlist
	    explode new-explode)
      (when (eq (left n) explode)
	(setf (left n) (sn-num 0))))
    (multiple-value-bind (new-tlist new-explode)
	(explode-traverse (right n) (1+ depth) traversal-list explode)
      (setf traversal-list new-tlist
	    explode new-explode)
      (when (eq (right n) explode)
	(setf (right n) (sn-num 0))))
    (when (and (leaf? n) (> depth 3) (not explode))
      (setf explode n)))
  (when (num? n) (setf traversal-list (cons n traversal-list)))
  (values traversal-list explode))

(defun explode (n)
  (multiple-value-bind (traversal-list explode)
      (explode-traverse n 0 nil nil)
    (setf traversal-list (reverse traversal-list))
    (when explode
      (let* ((left-index (position (left explode) traversal-list))
	     (right-index (1+ left-index))
	     (new-left-index (1- left-index))
	     (new-right-index (1+ right-index)))
	(when (>= new-left-index 0)
	  (let ((left-node (nth new-left-index traversal-list)))
	    (incf (num left-node) (num (left explode)))))
	(when (< new-right-index (length traversal-list))
	  (let ((right-node (nth new-right-index traversal-list)))
	    (incf (num right-node) (num (right explode)))))
	))
    (values n (not (null explode)))))

(defun reduce-sn (n)
  (let ((continue t))
    (loop while continue do
      (multiple-value-bind (n1 exploded?) (explode n)
	(setf n n1)
	(when (not exploded?)
	  (multiple-value-bind (n2 split?) (split n)
	    (setf n n2)
	    (setf continue split?)))))
    n))

(defparameter *input-file* #p"~/Projects/AoC2021/input/input18.txt")

(defun aoc18-load-input () (load-input *input-file*))

(defun aoc1801 (input)
  (let* ((nums (parse input))
	 (reduced (reduce #'(lambda (a b) (reduce-sn (add a b))) nums)))
    (print (print-snailfish reduced))
    (magnitude reduced)))

(defun test-pair (a b)
  (let ((reduced (reduce-sn (add a b))))
    (magnitude reduced)))

(defun all-permutations (l)
  (loop for a in l
	append (loop for b in (remove a l) collect
		      (list a b))))

(defun test-all (nums)
  (loop for p in nums collect (test-pair (first p) (second p))))

(defun aoc1802 (input)
  (let* ((perms (all-permutations input))
	 (nums (mapcar #'parse perms))
	 (mags (test-all nums)))
    (apply #'max mags)))



