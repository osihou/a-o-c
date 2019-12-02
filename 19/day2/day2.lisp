;;;Day 2

;;***FIRST***



(defun open-file (filename)
  (open filename :if-does-not-exist nil))

(defun close-file (inx)
  (close inx))


(defun read-and-print (inx)
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun decode-string (s)
 (labels ((f (lst) 
	     (when lst
	       (case (car lst)
		 (#\, (cons #\space (f (cdr lst))))
		 (otherwise  (cons (car lst) (f (cdr lst))))))))
   (coerce (f (coerce s 'list)) 'string)))


(defun decode-int (s)
  (loop :for (integer position) := (multiple-value-list
				     (parse-integer s
						    :start (or position 0)
						    :junk-allowed t))
	:while integer
	:collect integer))

(defun parse-input (in)
  (decode-int
    (decode-string
      (car (read-and-print in)))))



(defun add (program stack)
  "Executes the add command"
  (progn
    (setf 
      (nth (caddr stack) program) 
      (+ 
	(nth (car stack) program) 
	(nth (cadr stack) program)))
    (int-machine program (cdddr stack))))

(defun mult (program stack)
  "Executes the mult command"
  (progn
    (setf 
      (nth (caddr stack) program) 
      (* 
	(nth (car stack) program) 
	(nth (cadr stack) program)))
    (int-machine program (cdddr stack))))

(defun int-machine (program stack )
  "recursive int machine"
  (if (= (car stack) 99 )
    nil
  (case (car stack)
    (1 ( add program (cdr stack)))
    (2 ( mult program (cdr stack))))))

(defun restore (program p n)
  "restore computer to initial condition"
  (setf (nth p program) n))

(defvar in (open-file 'day2/input.md))

(defvar *programe* (parse-input in))

(defun noun-verb (n v)
  (progn
    (restore  *programe* 1 n)
    (restore  *programe* 2 v)))


(noun-verb 12 2)

(print *programe*)


(int-machine *programe* *programe*)
(print *programe*)

(close-file in)

(defun find-value (n v)
  (progn
    (setf *programe* (parse-input (open-file 'day2/input.md)))
    (noun-verb n v)
    (int-machine *programe* *programe*)
    (car *programe*)))
	

(print (find-value 12 2))

(defun active-search (var)
  (loop	named outer 
    	for n below 100
	do
	(loop for v below 100
	      when ( = (find-value n v) var)
	      do
		(return-from outer (cons n v)))))


(defvar *answer* (active-search 19690720))
(print *answer*)
(print (find-value (car *answer*)  (cdr *answer*)))


(defun calc (n v)
  (print (+ (* 100 n) v)))

(calc (car *answer*) (cdr *answer*))


;;***TESTS***
(print 'TESTS)

(defvar *test1* '( 1 0 0 0 99))
(int-machine *test1* *test1*)
(print *test1*)

(defvar *test2* '( 2 3 0 3 99))
(int-machine *test2* *test2*)
(print *test2*)

(defvar *test3* '( 2 4 4 5 99 0))
(int-machine *test3* *test3*)
(print *test3*)

(defvar *test4* '( 1 1 1 4 99 5 6 0 99))
(int-machine *test4* *test4*)
(print *test4*)




