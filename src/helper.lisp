(defun load-input (file-name)
  (loop for line in (uiop:read-file-lines file-name) collect line))

(defun input-to-2d-array (input)
  (let ((array (make-array (list (length input) (length (first input)))
			   :adjustable t)))
    (loop for y from 0 below (array-dimension array 0)
	  for row in input do
      (loop for x from 0 below (array-dimension array 1) do
	(setf (aref array y x) (digit-char-p (aref row x)))))
    array))
