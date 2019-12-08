;;Day 7


(defun all-permutations (list)
  "all posible permutations"
  (cond ((null list) nil)
	((null (cdr list)) (list list))
	 (t (loop for element in list
		  append (mapcar (lambda (x) (cons element x))
				 (all-permutations (remove element list)))))))


(defun open-file (filename)
  "Open a file"
  (open filename :if-does-not-exist nil))

(defun close-file (inx)
  "close file"
  (close inx))

(defun read-file (inx)
  "read file"
  (when inx
    (loop for line = (read-line inx nil)
		    while line
		    collect line)))

(defun parse-string (s)
  "parse input"
  (if s
    (if (position #\, s)
      (cons  (parse-integer (subseq s  0 (position #\, s)))
	    ( parse-string (subseq s (+ 1 (position #\, s))))))
      '()))

(defun sorted (lst predicate) 
  "is list sorted"
  (apply predicate lst))
   
(defun get-seq (num)
  "get sequance of numbers"
  (map 'list #'digit-char-p 
       (prin1-to-string num)))

(defun cdr-machine (num lst)
  (if (/= 0 num)
    (cdr-machine (1- num) (cdr lst))
    lst))

;;***MACHINE***

(defun pos-or-imm (seq stack program)
  (if (= seq 0)
    (nth  stack program)
     stack))


(defun add (program stack seq)
  "Executes the add command"
  (progn
    (setf 
      (nth (caddr stack) program) 
      (+ 
	(pos-or-imm (cadr seq) (car stack) program)
	(pos-or-imm (car seq) (cadr stack) program)))

    (int-machine program (cdddr stack))))

(defun mult (program stack seq)
  "Executes the mult command"
  (progn
    (setf 
      (nth (caddr stack) program) 
      (* 
	(pos-or-imm (cadr seq) (car stack) program)
	(pos-or-imm (car seq) (cadr stack) program)))
    (int-machine program (cdddr stack))))

(defun store (program stack)
  "Store parameter"
  (progn
    (setf
      (nth (car stack) program)
      (car *input*))
    (setf *input* (cdr *input*))
  (int-machine program (cdr stack))))

(defun output (program stack seq)
  "Print output"
  (progn
    (setf *input* (nconc *input* (list (pos-or-imm (cadr seq) (car stack) program))))
    (int-machine program (cdr stack))))

(defun jump-if-true (program stack seq)
  (if (/= (pos-or-imm (cadr seq) (car stack) program) 0)
    (int-machine program (cdr-machine (pos-or-imm (car seq) (cadr stack) program) program))
    (int-machine program (cddr stack))))

(defun jump-if-false (program stack seq)
  (if (= (pos-or-imm (cadr seq) (car stack) program) 0)
    (int-machine program (cdr-machine  (pos-or-imm (car seq) (cadr stack) program) program))
    (int-machine program (cddr stack))))


(defun less-than (program stack seq)
  (progn
    (if (< (pos-or-imm (cadr seq) (car stack) program) (pos-or-imm (car seq) (cadr stack) program))
      (setf
	(nth (caddr stack) program)
	1)
      (setf
	(nth (caddr stack) program)
	0))
    (int-machine program (cdddr stack))))
 
(defun equals (program stack seq)
  (progn
    (if (= (pos-or-imm (cadr seq) (car stack) program) (pos-or-imm (car seq) (cadr stack) program))
      (setf
	(nth (caddr stack) program)
	1)
      (setf
	(nth (caddr stack) program)
	0))
    (int-machine program (cdddr stack))))
   

(defun execute-command (program seq stack)
  (case (cadddr seq)
    (1 (add program stack seq))
    (2 (mult program stack seq))
    (4 (output program stack seq))
    (5 (jump-if-true program stack seq))
    (6 (jump-if-false program stack seq))
    (7 (less-than program stack seq))
    (8 (equals program stack seq))))

(defun resolve-command ( program seq stack)
  (case (length seq)
    (1 (execute-command program `(0 0 0 ,(car seq)) stack))
    (3 (execute-command program `(0 ,(car seq) 0 ,(caddr seq)) stack))
    (4 (execute-command program `(1 ,(cadr seq) 0 ,(cadddr seq)) stack))))

(defun int-machine (program stack )
  "recursive int machine"
  (if (= (car stack) 99 )
    nil
  (case (car stack)
    (3 ( store program (cdr stack)))
    (otherwise  
      (resolve-command program 
		       (get-seq (car stack)) (cdr stack))))))
		
(print (int-machine '(1 2 2 0 99) '(1 2 2 0 99 )))
;;AMPLIFIER

(defun get-program ()
  (parse-string (car (read-file (open-file 'day7/input.md)))))


(defvar *input* '(0))
(defvar *output* '())

(defun clean-memory ()
  (setf *input* '(0))) 

(defun amplifier-controller (program state )
  (if (car state) 
    (progn
      (setf *input* (cons (car state) *input*))
      (setf (car program) (int-machine (car program) (car program)))
      (amplifier-controller (cdr program) (cdr state) ))
  program))

(defun get-new-amp ()
  (loop
    for i
    from 0
    below 5
    collect (get-program)))

(defun feedback (program lst)
  (if (car program)
    (progn
      (amplifier-controller program lst)
      (feedback program lst))))
 
(defun feedback-loop (lst)
  (let ((amp-state (get-new-amp)))
    (feedback amp-state lst)))


(defun find-thrust (lst)
  (if (car lst)
    (progn
      (feedback-loop (car lst))
      (setf *output* (append *output* (list (cons *input* (list (car lst))))))
      (clean-memory)
      (find-thrust (cdr lst)))))

(clean-memory)

(defvar *lst1* (all-permutations '(0 1 2 3 4)))
(defvar in (open-file 'day7/input.md))
(defvar *programe*  (parse-string (car (read-file in))))



(find-thrust *lst1*)

(defvar *max-thrust* (loop for i
      in (cdr *output*)
      maximize (caar i)
      into biggest
      finally
      (return biggest)))

(loop
  for i in (cdr *output*)
  when (= (caar i) *max-thrust*)
  do (print i))



(defvar *lst2* (all-permutations '(5 6 7 8 9)))

`
(close-file in)

;***TESTS***
(print '***TESTS***)

(defun test-program (lst)
  (let ((test lst))
    (progn
      (int-machine lst test)
      (print lst))))

;(amplifier-controller '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0) '(4 3 1 2 0) )
;(clean-memory)
;(amplifier-controller '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0) '(0 1 2 3 4) )
;(clean-memory)


