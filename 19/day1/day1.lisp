;;;Day 1
(format t "Day 1~%")
(defvar *filename* 'day1/input.md )

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop :for line = (read-line inx nil)
	  :while line
	  :collect line)))

(defun get-input ()
  (mapcar #'parse-integer
	  (read-file (open-file *filename*))))

(defun sum-of-fuel (lst)
  (reduce #'+
	  (mapcar
	    (lambda (x)
	      (- (floor (/ x 3.0)) 2))
	    lst)))

(defun day1-first ()
  (format t "First: ~d~%" (sum-of-fuel (get-input))))


(day1-first)

(defun correction (value)
  (- (sum-of-fuels value) value))

(defun sum-of-fuels (value)
   (let ((fuel 0))
    (if (< value 0)
      0
      (setf fuel 
	    (+ fuel (+ value (sum-of-fuels  (sum-of-fuel (list value)))))))))

(defun day1-second ()
  (format t "~%Second: ~d~%" (reduce #'+ (mapcar #'correction (get-input)))))

(day1-second)

