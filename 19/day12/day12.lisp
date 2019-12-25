;;Day 12
(format t "Day 12~%")
(defparameter *filename* 'day12/input.md)

(defun flatten (lst &optional stack out)
  (cond ((consp lst) (flatten (rest lst) (cons (first lst) stack) out))
	(lst (flatten (first stack ) (rest stack) (cons lst out)))
	(stack (flatten (first stack) (rest stack) out))
	(t out)))

(defun derive-vtr (lst)
  (if (car lst)
    (cons
      (cons (parse-integer (car lst)) (parse-integer (cadr lst)))
      (derive-vtr (cddr lst)))))


(defun flat-vtr (inpt)
  (derive-vtr (flatten inpt)))


(defun cpy-lst (lst)
  (mapcar (lambda (x)
	    (mapcar #'copy-list x)) lst))

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun parse-position (s)
  (let ((sq (subseq s (+ 1 (position #\= s)))))
    sq))

(defun parse-string-x (sq)
    (subseq sq  0   (position #\, sq)))

(defun parse-string-y (sq)
  (parse-string-x (parse-position sq)))

(defun parse-string-z (sq)
  (let ((seq (parse-position (parse-position sq))))
    (subseq seq 0 (position #\> seq))))

(defun parse-xyz (seq)
  (mapcar #'parse-integer 
	  (list (parse-string-x seq)
		(parse-string-y seq)
		(parse-string-z seq))))
	

(defun read-inputs (file)
  (reverse (cdr (reverse (read-file (open-file file))))))

(defun get-values()
  (loop :for line 
	:in (read-inputs *filename*)
	:collect (list (parse-xyz (parse-position line)) (list 0 0 0 ))))

      
(defun coordinates-selector (x)
  (lambda (planet)
    (list (list (nth x (first planet)))
	  (list (nth x (second planet))))))

(defun gravity (planet-f planet-s)
  (loop :for p1
	:in (first planet-f)
	:for p2
	:in (first planet-s)
	:for index
	:upfrom 0
	:for delta := (cond
			(( > p1 p2) -1)
			((< p1 p2) 1)
			(t 0))
	:do 
	(incf (nth index (second planet-f)) delta)
	(decf (nth index (second planet-s)) delta)))


(defun simulate (planets)
  (loop :for sub
	:on planets
	:for planet-f := (first sub)
	:do 
	(loop :for planet-s
	      :in (rest sub)
	      :do
	      (gravity planet-f planet-s ))
	(setf (first planet-f) 
	      (mapcar #'+ (first planet-f) (second planet-f))))
  planets)

(defun total-energy (planets)
  (loop :for planet
	:in planets
	:sum
	(* (reduce #'+ (mapcar #'abs (first planet)))
	   (reduce #'+ (mapcar #'abs (second planet))))))

(defun day12-first()
  (format t "First: ~d~%" (loop :with coordinates := (get-values)
				:repeat 1000
				:do (simulate coordinates)
				:finally (return (total-energy coordinates)))))


(day12-first)

(defun detect (planets)
  (loop :for current := (simulate (cpy-lst planets))
	:then (simulate current)
	:for step-count
	:upfrom 1
	:until (equalp current planets)
	:finally (return step-count)))


(defun day12-second ()
  (let ((planets (get-values)))
    (format t "Second: ~d~%"  (apply #'lcm 
				     (mapcar (lambda (c) 
						     (detect 
						       (mapcar (coordinates-selector c) planets)))
					     '(0 1 2))))))



(day12-second)
