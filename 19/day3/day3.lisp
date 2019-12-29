;;Day 3
(format t "Day 3~%")
(defvar *filename* 'day3/input.md)

(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun read-file (inx)
  (when inx
    (loop :for line = (read-line inx nil)
	  :while line
	  :collect line)))

(defun parse-string (s)
  (if s
    (if (position #\, s)
      (cons  (subseq s  0 (position #\, s)) 
	    ( parse-string (subseq s (+ 1 (position #\, s))))))
      '()))


(defun parse-command (s)
  (let (( str (car s)))
  (if str
    (cons 
      (cons (intern (subseq str 0 1)) 
	    (parse-integer(subseq str 1))) 
      (parse-command (cdr s)))
    '())))

(defun move (command value)
  (case  (car command)
    ('U (cons (car value) (+ (cdr value) (cdr command))))
    ('D (cons (car value) (- (cdr value) (cdr command))))
    ('R (cons (+ (car value) (cdr command)) (cdr value)))
    ('L (cons (- (car value) (cdr command)) (cdr value)))))

(defun move-line ( command value )
  (if (car command)
    (cons 
      (cons  value (list (move (car command) value)))
      (move-line  (cdr command) (move (car command) value)))
    '()))


(defvar in (open-file 'day3/input.md))

(defvar *memory* (read-file in))

(defun create-line (commands)
  (move-line commands '(0 . 0)))

(defvar *line1*  (create-line (parse-command (parse-string (car *memory*)))))
(defvar *line2* (create-line (parse-command (parse-string (cadr *memory*)))))

(defun get-length (line)
  (+ (abs (- (caar line) (caadr line))) 
     (abs ( - (cdar line) (cdadr line)))))

(defun get-relative-length (line1 line2)
  (+ (get-length (list (car line1) (car line2)))
     (get-length (list (cadr line1) (cadr line2)))))

(defun manchatan (line1 line2)
  (if (and line1 line2)
      (if ( = ( + ( get-length line1) (get-length  line2)) (get-relative-length  line1 line2))
	(list  line1  line2) 
    '())))

(defun point (lst)
  (if (= (caaar lst) (caadar lst))
    (list  (cons (caaar lst) (cdaadr lst)))
    (list  (cons (caaadr lst) (cdaar lst)))))


(defun get-distance (pt)
  (+ (abs (car pt)) (abs (cdr pt))))

(defvar *ln* '())

(defun cycle-through (line-1 line2)
  (loop :for i 
	:in line-1
	:do (loop :for j 
		    :in line2
		    :do  
			  (let ((x (manchatan i  j)))
			    (if x
			      (setf *ln* (append *ln*  (point x))))))))

(cycle-through *line1* *line2*)

(defun day3-first ()
  (format t "First: ~d~%" (loop :for i
			      :in (cdr (mapcar #'get-distance *ln*))
			      :minimize i)))


(day3-first)
