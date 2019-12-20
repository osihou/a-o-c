;;DAY 4
(format t "Day 4~%")

(defvar *start* 171309)
(defvar *finish* 643603)


   
(defun sorted (lst predicate) (apply predicate lst))
   
(defun get-seq (num)
  (map 'list #'digit-char-p (prin1-to-string num)))

(defun dupes (lst)
  (cond ((null lst) '())
	((member (car lst) (cdr lst)) (cons (car lst) (dupes (cdr lst))))
	(t (dupes (cdr lst)))))

(defvar *ln2* '())

(defun gen-first-interval ( begin end )
  (loop :for i
	:from begin
	:to end
	:when (and (sorted (get-seq i) '<=)  (dupes (get-seq i)))
	:collect i))



(defun day4-first ()
  (format t "First: ~d~%" (length (gen-first-interval *start* *finish*))))

(day4-first)

(defun gen-second-interval ( begin end )
  (loop :for i
	:from begin
	:to end
	:when (and (sorted (get-seq i) '<=) 
		   (and (dupes (get-seq i)) 
			(set-difference (dupes (get-seq i)) 
					(dupes (dupes (get-seq i))))))
	:collect i))



(defun day4-second ()
  (format t "Second: ~d~%" (length (gen-second-interval *start* *finish*))))

(day4-second)
