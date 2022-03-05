(defun aoc02-load-input ()
  (uiop:read-file-lines "../input/input02.txt"))

(defun aoc02-parse-input (input)
  (map 'list #'(lambda (x) (let* ((cmd (uiop:split-string x :separator " ")))
			     (setf (second cmd) (parse-integer (second cmd)))
			     cmd)) input))

(defun aoc0201 ()
  (let* ((pos 0)
	 (depth 0)
	 (input (aoc02-parse-input (aoc02-load-input))))
    (dolist (cmd input)
      (cond
	((string-equal (first cmd) "forward") (incf pos (second cmd)))
	((string-equal (first cmd) "down") (incf depth (second cmd)))
	((string-equal (first cmd) "up") (decf depth (second cmd)))))
    (* pos depth)))

(defun aoc0202 ()
  (let* ((pos 0)
	 (depth 0)
	 (aim 0)
	 (input (aoc02-parse-input (aoc02-load-input))))
    (dolist (cmd input)
      (cond
	((string-equal (first cmd) "forward")
	 (progn
	   (incf pos (second cmd))
	   (incf depth (* aim (second cmd)))))
	((string-equal (first cmd) "down") (incf aim (second cmd)))
	((string-equal (first cmd) "up") (decf aim (second cmd)))))
    (* pos depth)))

    
  
