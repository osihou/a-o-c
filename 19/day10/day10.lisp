;;Day 10
(format t "Day 10")
(defvar *filename* 'day10/day10.lisp)
(setq *read-default-float-format* 'long-float)

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun parse-string (s)
  (if s
    (if (position #\, s)
      (cons  (parse-integer (subseq s  0 (position #\, s)))
	    ( parse-string (subseq s (+ 1 (position #\, s))))))
      '()))

(defun sorted (lst predicate) 
  (apply predicate lst))
   
(defun get-seq (num)
  (map 'list #'digit-char-p 
       (prin1-to-string num)))

(defun open-and-read (file)
  (read-file
	 (open-file file)))

(defun get-letters (wrd)
  (loop :for chr 
	:across wrd
	:collect chr))

(defun get-coordinates (input)
  (loop :for wrd 
	:in input
	:for i 
	:from 0
	:collect (loop :for ltr 
		       :in (get-letters wrd)
		       :for j 
		       :from 0
		       :if (eq ltr #\#)
		       :collect (cons i j))))

(defun vtr-length (vtr)
  (let ((x (car vtr)) (y (cdr vtr)))
    (sqrt (+ (* x x) (*  y y)))))

(defun get-vector (point1 point2)
  (let ((x1 (car point1)) 
	(y1 (cdr point1)) 
	(x2 (car point2)) 
	(y2 (cdr point2)))
    (cons (- x2 x1) (-  y2 y1))))


(defun angle (vtr)
  (let ((value
	  (- (/ pi 2) (atan (- (cdr vtr)) (car vtr)))))
    (if (> 0 (round value 1e-5))
      (+ value (* 2 pi))
      value)))

(print (angle '(0 . 1)))
(defun flatten (lst &optional stack out)
  (cond ((consp lst) (flatten (rest lst) (cons (first lst) stack) out))
	(lst (flatten (first stack ) (rest stack) (cons lst out)))
	(stack (flatten (first stack) (rest stack) out))
	(t out)))

(defun derive-vtr (lst)
  (if (car lst)
    (cons
      (cons (car lst) (cadr lst))
      (derive-vtr (cddr lst)))))


(defun flat-vtr (inpt)
  (derive-vtr (flatten inpt)))

(defun angles (point lst)
  (if (car lst)
    (if (equal point (car lst))
      (angles point (cdr lst))
      (cons
	(angle (get-vector point (car lst)))
	(angles point (cdr lst))))))

(defun unique-values (lst)
  (if (car lst)
    (if (member (car lst) (cdr lst))
      (unique-values (cdr lst))
      (cons
	(car lst)
	(unique-values (cdr lst))))))


(defun length-of-unique (point lst)
  (length 
    (unique-values
      (angles point lst))))

(defun get-visible (lst)
  (loop :for i 
	:in lst
	:collect (length-of-unique i lst)))

(defvar *count* (get-visible (flat-vtr (get-coordinates (open-and-read 'day10/input.md)))))

;(defvar *biggest* (loop :for i
;	     :in (get-visible (flat-vtr (get-coordinates (open-and-read 'day10/input.md))))
;	     :maximize i
;	     :into biggest
;	     :finally (return biggest)))
		


(defun day10-first ()
  (format t "First: ~d~%" (loop :for i
				:in (get-visible (flat-vtr (get-coordinates (open-and-read *filename*))))
				:maximize i
				:into biggest
				:finally (return biggest))))

(day10-first)

;(print *biggest*)
;(defvar *len* (- (length *count*) (length (member *biggest* *count*))))
;(print (nth *len* (flat-vtr (get-coordinates (open-and-read 'day10/input.md)))))
;(defvar *sorted* (sort (angles '(29 . 26) (flat-vtr (get-coordinates (open-and-read 'day10/input.md))))  #'<))
;(defvar *usorted* (append (member  pi  *sorted*) (set-difference *sorted* (member  pi  *sorted*))))

;(print *usorted*)












