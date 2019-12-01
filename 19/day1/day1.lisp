;;;Day 1

;;;***FIRST***

(defun sum-of-fuel (lst)
  "Calculate the needed fuel for module"
  (reduce #'+
	  (mapcar
	    (lambda (x)
	      (- (floor (/ x 3.0)) 2))
	    lst)))
;;tests
(print 'TEST)
(print (sum-of-fuel '(12 14 1969 100756)))

(print '***SECOND***)
;;;***SECOND***
(defun correction (value)
  (- (sum-of-fuels value) value))

(defun sum-of-fuels (value)
  "Recursive function that calculates the needed fuels"
   (let ((fuel 0))
    (if (< value 0)
      0
      (setf fuel 
	    (+ fuel (+ value (sum-of-fuels  (sum-of-fuel (list value)))))))))

(defun read-and-print (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (print 
	(reduce #'+
	(mapcar #'correction
	(mapcar #'parse-integer 
		(loop for line = (read-line in nil)
		      while line  
		      collect line)))))
      (close in))))

(read-and-print "day1/input.md")

;;tests
(print 'TEST)
(print (correction 14))
(print (correction 100756))
