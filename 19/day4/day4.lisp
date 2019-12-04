

;;***DAY4***

;;***FIRST***
   
(defun sorted (lst predicate) (apply predicate lst))
   
(defun get-seq (num)
  (map 'list #'digit-char-p (prin1-to-string num)))

(defun dupes (lst)
  (cond ((null lst) '())
	((member (car lst) (cdr lst)) (cons (car lst) (dupes (cdr lst))))
	(t (dupes (cdr lst)))))
(defvar *ln1* '())
(defvar *ln2* '())

(defun gen-interval-1 ( begin end )
  (loop for i
	from begin
	to end
	when (and (sorted (get-seq i) '<=)  (dupes (get-seq i)))
	do
	(setf *ln1* (append *ln1* (list i)))))


(gen-interval-1 171309 643603)
(print '***FIRST***)
(print (length *ln1*))


;;***SECOND***
(defun gen-interval-2 ( begin end )
  (loop for i
	from begin
	to end
	when (and (sorted (get-seq i) '<=) (and (dupes (get-seq i)) ( set-difference (dupes (get-seq i)) (dupes (dupes (get-seq i))))))
	do
	(setf *ln2* (append *ln2* (list i)))))



(gen-interval-2 171309 643603)
(print '***SECOND***)
(print (length *ln2*))






;;***TESTS***
(print '***TESTS***)
(print (dupes '(2 2 3 3 3 3)))
(print (set-difference '(1 1 1 2 2) '(1 1)))

(print (map 'list #'digit-char-p (prin1-to-string  1322)))

;(map 'list (lambda (c) (or ( digit-char-p c) '-)) (prin1-to-string 1232))

(defun number-to-list (n &optional tail)
  (if (zerop n)
    (or tail '(0))
    (multiple-value-bind (val rem)
      (floor n 10)
      (number-to-list val (cons rem tail)))))


(print (sorted '(5 2 3) '<= ))
