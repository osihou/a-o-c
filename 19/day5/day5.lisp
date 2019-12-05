;;Day 5

;;***FIRST*** 
;;For 1
;;**SECOND**
;;For 5
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

;0 - position mode
;1 - immediate mode


; ABCDE
; 	DE - two-digit opcode
;	C - mode of 1st parameter
; 	B - mode of 2nd parameter
;	A - mode of 3rd parameter

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
      (progn 
	(print '***INPUT***)
	(read)))
  (int-machine program (cdr stack))))

(defun output (program stack seq)
  "Print output"
  (progn
    (print (pos-or-imm (cadr seq) (car stack) program))
    (int-machine program (cdr stack))))

(defun cdr-machine (num lst)
  (if (/= 0 num)
    (cdr-machine (1- num) (cdr lst))
    lst))

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
		

(defvar in (open-file 'day5/input.md))

(defvar *programe*  (parse-string (car (read-file in))))

(print (length *programe*))
;(print *programe*)

(int-machine *programe* *programe*)


(close-file in)

;***TESTS***
(print '***TESTS***)

(defun test-program (lst)
  (let ((test lst))
    (progn
      (int-machine lst test)
      (print lst))))

;(test-program '(1101 100 -1 4 0))
;(test-program '(1002 4 3 4 33))
;(test-program '(104 1 99))
;(test-program '(4 1 99))
;(test-program '(3 3 14 4  99))
;(test-program '(3 9 8 9 10 9 4 9  99 -1 8))
;(test-program '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9))
;(test-program '(3 3 1107 -1 8 3 4 3 99))
;(test-program '(3 3 1105 -1 9 1101 0 0 12 4 12 99 1))
;(test-program '(3 3 1108 -1 8 3 4 3 99))
;(test-program '(3 9 7 9 10 9 4 9 99 -1 8))
;(test-program '(3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 114 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))
;(test-program '(1107 9 1 5 14 0 99))
